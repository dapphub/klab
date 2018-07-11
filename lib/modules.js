const _ = require("lodash");
const clc = require('cli-color');
// const { formatStep } = require("./formatter.js");
const { formatMemory } = require("./unwrapper.js");
const Constraints = require("./constraints.js");
const S = require("./state.js");
const Rule = require("./rule.js");
const kast = require("./kast.js");
const Constraint = require("./constraints.js")
const format = require("./format.js")
const { genBehaviour } = require("./behavior.js");
const evmv = require("./evmv.js");

const hide_color = 244;
const show_color = 255;


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
  },
  constraint: {
    view: (state) => {
      return S.const(state);
    },
  },
  // TODO - evm specific
  evm: {
    view: evmv
  },
  rule: {
    view: (state) => {
      let id = state.path[state.path.length - 1].step.rule;
      let rule = state.nodes[id] && JSON.stringify(state.nodes[id].term.att, false, 2) || "";
      return rule;
    },
  },
  behaviour: {
    view: (state) => {
      const styles = node => !node && 22
        || node.branching && 215
        || node.in_history && show_color // 77
        || node.active && 244
        || 234

      const behaviour = genBehaviour(state);
      const table = format.foldPreorder({
        tree: behaviour,
        loc: "index",
        styles
      })
      return format.formatDb(table, ["index", "success", "deltaC"], styles)
    },
  },
  debug: {
    view: (state) => {
      let debug = {
        bla: state.rules
        // frontier: state.behaviour.frontier
        // proofid: state.proofid,
        // id: S.id(state),
        // steps: state.steps,
        // finished: state.finished
      };
      return JSON.stringify(debug, false, 2);
    },
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
  },
  z3feedback: {
    view: (state) => {
      let nodeid = S.id(state);
      // return JSON.stringify(state.z3feedback[nodeid] || {}, false, 2)
      if (! (nodeid in state.z3feedback)) return "";
      let rs = state.z3feedback[nodeid]
        .filter(msg => msg.rule !== "NORULE")
        .reduce((a, r) => ({
          ...a,
          [r.rule]: [...(a[r.rule] || []), r]
        }), {})

      return Object.keys(rs)
        .map(ruleid => {
          let z3s = rs[ruleid];
          if(ruleid in state.rules) {
            // todo parsing is server
            // let rO = Rule.parseRule(state.nodes[ruleid].term.att);
            // return ruleid
            //   + " "
            //   + (ruleid in state.rules)
            //   + " "
            //   + JSON.stringify(state.rules[ruleid], false, 2)

            return Rule.formatRule(state.rules[ruleid])
              + "\n"
              + z3s.map(({query, result}) => {
                let resultStr = result in state.nodes
                  && state.nodes[result].term.token
                  || "";
                return ` ${query} ${resultStr}`;
              }).join("\n")
          }
          return ".";
        }).join("\n\n")
          // rule + " " +
          // query + " " +
          // (result in state.nodes && state.nodes[result].term.token || "" ))
          //
          //
          //
        // .map(id => (state.z3feedbackdata || {})[id] || "")
        // .filter(o => !!o)
        // .map(o => {
        //   let rhs        = kast.formatConstraint(o.rhs)
        //   let z3QueryId  = o.queryId
        //   let z3Result   = o.result.term.token
        //   return "result: " + clc.xterm(210)(z3Result)   + "\n"
        //    + "checking:"                             + "\n"
        //    + "    " + rhs + "\n";
        // })
        // .join("\n")
      // return z3feedback;
    },
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
  },
  help: {
    view: state => {
      return "TODO";
    },
  },
  log: {
    view: state => {
      return state.error || ""
    },
  }
}

module.exports = view
