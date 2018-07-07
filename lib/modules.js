const _ = require("lodash");
const clc = require('cli-color');
// const { formatStep } = require("./formatter.js");
const { formatMemory } = require("./unwrapper.js");
const Constraints = require("./constraints.js");
const util = require("./util.js");
const S = require("./state.js");
const kast = require("./kast.js");
const Constraint = require("./constraints.js")
const format = require("./format.js")
const { genBehaviour } = require("./behavior.js");
const evmv = require("./evmv.js");

const formatRule = rule => {
  if(!rule) return "?"
  let string = clc.xterm(0)(`${rule.filepath} ${rule.from}-${rule.to}\n     `) + util.indent(rule.string, 2);
  string = string.shorten(string, 6)
  return string;
}

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
    toggle: ["t", "T"]
  },
  constraint: {
    view: (state) => {
      return S.const(state);
    },
    toggle: ["c", "C"]
  },
  // TODO - evm specific
  evm: {
    view: evmv,
    toggle: ["e", "E"],
    // default: true
  },
  rule: {
    view: (state) => {
      let id = state.path[state.path.length - 1].step.rule;
      let rule = state.nodes[id] && JSON.stringify(state.nodes[id].term.att, false, 2) || "";
      return rule;
    },
    toggle: ["r", "R"]
  },
  behaviour: {
    view: (state) => {
      const styles = node => !node && 22
        || node.branching && 215
        || node.in_history && 77
        || node.active && show_color
        || hide_color

      const behaviour = genBehaviour(state);
      const table = format.foldPreorder(behaviour, "index", "", true, styles)
      return format.formatDb(table, ["index", "success", "deltaC"], styles)
    },
    toggle: ["b", "B"]
  },
  debug: {
    view: (state) => {
      let debug = {
        bla: state.z3feedback
        // frontier: state.behaviour.frontier
        // proofid: state.proofid,
        // id: S.id(state),
        // steps: state.steps,
        // finished: state.finished
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
      let nodeid = S.id(state);
      // return JSON.stringify(state.z3feedback[nodeid] || {}, false, 2)
      if (! (nodeid in state.z3feedback)) return "";
      let z3feedback = state.z3feedback[nodeid]
        .map(({query, result}) =>
          query + " " + (result in state.nodes && state.nodes[result].term.token || "" ))
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
  log: {
    view: state => {
      return state.error || ""
    },
    toggle: ["l", "L"]
  }
}

module.exports = view
