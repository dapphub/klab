const _ = require("lodash");
const clc = require('cli-color');
// const { formatStep } = require("./formatter.js");
const { formatMemory } = require("./unwrapper.js");
const { getCodeStringFromPc } = require("../lib/srchandler.js");
const Constraints = require("./constraints.js");
const util = require("./util.js");
const S = require("./state.js");
const K = require("./k.js");

const formatRule = rule => {
  if(!rule) return "?"
  let string = clc.xterm(0)(`${rule.filepath} ${rule.from}-${rule.to}\n     `) + util.indent(rule.string, 2);
  string = string.shorten(string, 6)
  return string;
}
// TODO -- ??

const genBehaviour = (state) => {
  let behaviour = "";
  let frontier = [{
    id: state.initt,
    path: "",
    leave: false
  }];
  let leaves = [];
  let terminate = false;
  let _visited = {
    [state.initt]: true
  };
  while (frontier.length > 0) {
    let next_frontier = [];
    frontier.forEach(e => {
      let next_steps = (state.edges[e.id] || []);

      if(next_steps.length == 0) {
        e.leave = true;
        if( e.id === state.targett ) e.path += clc.green(" ✓");
        if( e.id in state.crash) e.path += " 💥";
        if( e.id in state.exception ) e.path += clc.red(" ✗e")
        if( e.id in state.revert ) e.path += clc.red(" ✗r")
        if( e.id in state.halt ) e.path += clc.blue(" ⊥")
        if( e.id in state.end ) e.path += clc.blue(" ⊥")
        leaves.push(e);
        return null;
      }

      let next_frontier_steps = next_steps
        .map((step, i) => ({
          id: step.to,
          path: e.path + (next_steps.length > 1 ? i + "." : ""),
          leave: false
        }))
      next_frontier = next_frontier.concat(next_frontier_steps);
    });
    frontier = next_frontier;
  }
  behaviour = leaves
  .map(branch => branch.path)
  .sort()
  .join("\n")
  return behaviour;
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
      let test = K.getO(k, req)

      return JSON.stringify(test, false, 2);
    },
    toggle: ["t", "T"]
  },
  constraint: {
    view: (state) => {
      let id = state.path[state.path.length - 1].step.to.split("_")[1]
      let cs = K.formatConstraint(state.nodes[id])
      return Constraints.clean(cs);
    },
    toggle: ["c", "C"]
  },
  // TODO - evm specific
  source: {
    view: (state) => {
      let id = S.term_id(state)
      let k = state.nodes[id].term;
      let pc = K.get(k, "ethereum.evm.callState.pc");
      return getCodeStringFromPc(state.config, parseInt(pc), true)
    },
    toggle: ["s", "S"]
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
      let behaviour = genBehaviour(state);
      let format_path = "root.";
      state.path.forEach(s => {
        if(s.type === "branch") format_path += s.branch + ".";
      });
      let filtered = behaviour
        .split("\n")
        .filter(b => b.indexOf(format_path.slice(5)) == 0)
        .map(b => clc.green(format_path.slice(5)) + b.slice(format_path.length - 5))
        .join("\n")
      return filtered;
    },
    toggle: ["b", "B"]
  },
  debug: {
    view: (state) => {
      let debug = {
        proofid: state.proofid,
        path: state.path,
        id: S.id(state),
        steps: S.steps(state),
        edges: state.edges
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
          let rhs        = K.formatConstraint(o.rhs)
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
      let kcell = K.get(k, "k")
      return kcell
        // .split("~>")
        // .join("\n~>")
    },
    toggle: ["k", "K"],
    default: true
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
      let stack = K.get(k, "ethereum.evm.callState.wordStack");
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
