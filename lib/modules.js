const _ = require("lodash");
const clc = require('cli-color');
// const { formatStep } = require("./formatter.js");
const { formatMemory } = require("./unwrapper.js");
const { getCodeStringFromPc } = require("../lib/srchandler.js");
const Constraints = require("./constraints.js");
const util = require("./util.js");
const S = require("./state.js");
const kast = require("./kast.js");
const Constraint = require("./constraints.js")

const formatRule = rule => {
  if(!rule) return "?"
  let string = clc.xterm(0)(`${rule.filepath} ${rule.from}-${rule.to}\n     `) + util.indent(rule.string, 2);
  string = string.shorten(string, 6)
  return string;
}

const genBehaviour = (state) => {
  // let frontier = [{
  //   id: state.initt,
  //   path: "",
  //   leave: false
  // }];
  const behaviour = {
    id: state.initt,
    children: [],
    leave: false,
    height: 0
  }
  var frontier = [
    behaviour
  ]
  // let leaves = [];
  const visited = {
    [state.initt]: true
  }
  var height = 0;
  const potential_leaves = [];

  while (frontier.length > 0) {
    let next_frontier = [];
    height ++;
    frontier.forEach((node, i) => {
      let steps = (state.edges[node.id] || []);
      if(steps.length == 0) {
        potential_leaves.push(node);
        // node.leave = true;
      }
      if(steps.length == 1) {
        node.id = steps[0].to;
        node.height++;
        next_frontier = [...next_frontier, node];
      }
      if(steps.length > 1) {
        let children = steps.map(step => ({
          id: step.to,
          leave: false,
          children: [],
          height: node.height + 1,
          step,
          deltaC: Constraint.deltaC(state, step)
        }));
        node.children = children;
        next_frontier = [...next_frontier, ...children]
      }
    })
    frontier = next_frontier;
  }
  potential_leaves
    .forEach(node => {
      if(node.height < height - 2) {
        node.leave = true;
      }
    })

  // while (frontier.length > 0) {
  //   let next_frontier = [];
  //   frontier.forEach(e => {
  //     let next_steps = (state.edges[e.id] || []);
  //
  //     if(next_steps.length == 0) {
  //       e.leave = true;
  //       if( e.id === state.targett ) e.path += clc.green(" ✓");
  //       if( e.id in state.crash) e.path += " 💥";
  //       if( e.id in state.exception ) e.path += clc.red(" ✗e")
  //       if( e.id in state.revert ) e.path += clc.red(" ✗r")
  //       if( e.id in state.halt ) e.path += clc.blue(" ⊥")
  //       if( e.id in state.end ) e.path += clc.blue(" ⊥")
  //       leaves.push(e);
  //       return null;
  //     }
  //
  //     let next_frontier_steps = next_steps
  //       .map((step, i) => ({
  //         id: step.to,
  //         path: e.path + (next_steps.length > 1 ? i + "." : ""),
  //         leave: false
  //       }))
  //     next_frontier = next_frontier.concat(next_frontier_steps);
  //   });
  //   frontier = next_frontier;
  // }
  return behaviour;
  // .map(branch => branch.path)
  // .sort()
  // .join("\n")
}

const view = {
  term: {
    view: (state) => {
      let id = state.path[state.path.length - 1].step.to.split("_")[0]
      let k = state.nodes[id].term;
      let req = {
        k: "k",
        stack: "ethereum.evm.callState.wordStack",
        mem: "ethereum.evm.callState.localMem"
      }
      let test = kast.getO(k, req)

      return JSON.stringify(test, false, 2);
    },
    toggle: ["t", "T"]
  },
  constraint: {
    view: (state) => {
      return S.const(state);
    },
    toggle: ["c", "C"]
  },
  // TODO - evm specific
  source: {
    view: (state) => {
      let id = S.term_id(state)
      let k = state.nodes[id].term;
      let pc = kast.get(k, "ethereum.evm.callState.pc");
      return getCodeStringFromPc(state.config, parseInt(pc), true)
    },
    toggle: ["s", "S"],
    // default: true
  },
  rule: {
    view: (state) => {
      let id = state.path[state.path.length - 1].step.rule;
      let rule = state && state.rules && state.rules[id] && formatRule(state.rules[id]) || "";
      return rule;
    },
    toggle: ["r", "R"]
  },
  behaviour: {
    view: (state) => {
      const unfoldBehaviour = _o => {
        const o = {
          indent: "",
          selected: true,
          ..._o
        };
        let hide_color = 244;
        let show_color = 255;
        return o.node.children
          .map((c, i) => {
            let cs = unfoldBehaviour({
              node: c,
              path: o.path.slice(1),
              selected: o.path[0] === i,
              indent: o.indent + "  ",
              depth: o.depth - 1
            })
            let is_selected = o.selected && o.path.length > 0 && o.path[0] == i;
            let is_branching = S.id(state) == o.node.id;
            let str = o.indent
              + (is_branching ? "*" : " ")
              + i
              + ".  "
              + "  ".repeat(o.depth)
              + c.deltaC
              + (cs != "" ? "\n" : "")
              + cs;
            let color = is_selected && 77
              || o.path.length == 0 && is_branching && 215
              || o.path.length == 0 && show_color
              || hide_color
              ;
            return clc.xterm(color)(str);
          })
          .join("\n")
      }
      const behaviour = genBehaviour(state);
      const getDepth = node => {
        if(node.children.length == 0) return 0;
        return Math.max.apply(null, node.children.map(c => getDepth(c) + 1))
      }
      const depth = getDepth(behaviour)
      const clean_path = state.path
        .filter(act => act.type == "branch")
        .map(act => act.branch)
      return unfoldBehaviour({
        node: behaviour,
        path: clean_path,
        depth
      });
    },
    toggle: ["b", "B"]
  },
  debug: {
    view: (state) => {
      let debug = {
        proofid: state.proofid,
        path: state.path,
        id: S.id(state),
        rules: state.rules
      };
      return JSON.stringify(debug, false, 2);
    },
    toggle: ["d"]
  },
  memory: {
    view: (state) => {
      let id = state.path[state.path.length - 1].step.to;
      let term = state.nodes[id].term;
      let memory_string = term.generatedTop.ethereum.evm.callState.localMem;
      return memory_string.split("\n").map(s => s.trim()).join("\n");
      // let formatted_memory = formatMemory(memory_string);
      // return formatted_memory;
    },
    toggle: ["m", "M"]
  },
  z3feedback: {
    view: (state) => {
      let nodeId = S.id(state);
      if (! (nodeId in state.z3feedback)) return ""
      let z3feedback = state.z3feedback[nodeId]
        .map(id => (state.z3feedbackdata || {})[id] || "")
        .filter(o => !!o)
        .map(o => {
          let rhs        = kast.formatConstraint(o.rhs)
          let z3QueryId  = o.queryId
          let z3Result   = o.result.term.token
          return "result: " + clc.xterm(210)(z3Result)   + "\n"
           + "checking:"                             + "\n"
           + "    " + rhs + "\n";
        })
        .join("\n")
      return z3feedback;
    },
    toggle: ["z", "Z"]
  },
  kcell: {
    view: state => {
      let id = S.term_id(state)
      let k = state.nodes[id].term;
      let kcell = kast.get(k, "k")
      return kcell
        // .split("~>")
        // .join("\n~>")
    },
    toggle: ["k", "K"]
  },
  help: {
    view: state => {
      return "TODO";
    },
    toggle: ["h","H"],
  },
  stack: {
    view: state => {
      let id = S.term_id(state)
      let k = state.nodes[id].term;
      let stack = kast
        .get(k, "ethereum.evm.callState.wordStack")
        .split("\n")
      stack = Constraints.clean(stack);
      return stack
        .split(":")
        .map((s, i) => "#" + i + "  " + s.trim())
        .join("\n")
    },
    toggle: ["q"]
  }
}

module.exports = view
