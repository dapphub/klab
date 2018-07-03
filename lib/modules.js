const _ = require("lodash");
const clc = require('cli-color');
// const { formatStep } = require("./formatter.js");
const { formatMemory } = require("./unwrapper.js");
const {
  getCodeStringFromPc,
} = require("../lib/srchandler.js");
const Constraints = require("./constraints.js");
const util = require("./util.js");
const S = require("./state.js");
const kast = require("./kast.js");
const Constraint = require("./constraints.js")
const format = require("./format.js")
const { genBehaviour } = require("./behavior.js");

const r = s => " ".repeat(4 - s.length) + s
const hex = n => n.toString(16).length % 2 == 0 ? n.toString(16) : "0" + n.toString(16);
const formatRule = rule => {
  if(!rule) return "?"
  let string = clc.xterm(0)(`${rule.filepath} ${rule.from}-${rule.to}\n     `) + util.indent(rule.string, 2);
  string = string.shorten(string, 6)
  return string;
}

const hide_color = 244;
let show_color = 255;


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
    view: (state) => {
      let id = S.term_id(state)
      let k = state.nodes[id].term;
      let pc = kast.get(k, "ethereum.evm.callState.pc");
      let src = getCodeStringFromPc(state.config, parseInt(pc), true, state.config.pc_to_inst_map, state.config.srcmapArr);
      let inst = state.config.pc_to_inst_map[pc];
      let from = Math.max(0, inst - 5);
      let to   = Math.min(state.config.instructions.length, from + 10);
      let header = ("    PC    ID  Opcode" + " ".repeat(22) + "Stack");
      let str = state.config.instructions
        .slice(from, to)
        .map((s, i) => (from + i === inst ? "> " : "  ") + r(hex(state.config.inst_to_pc[from + i])) + "  " + r(hex(from + i)) + "  " + (s.length > 20 ? s.slice(0, 20) + "..." : s))
        .map((s,i) => clc.xterm(from + i === inst ? show_color : hide_color)(s))
        .join("\n")
      let a1 = str.split("\n").map(l => l + " ".repeat(42 - clc.getStrippedLength(l)))

      let stack = kast
        .get(k, "ethereum.evm.callState.wordStack")
        .split("\n")
      let stackArr = Constraints.clean(stack)
        .split(":")
        .map((s, i) => hex(i) + "  " + s.trim())
        .slice(0, -1)
      stack = ( stackArr.length > 10 ? stackArr.slice(0, 9).concat(["..."]) : stackArr ).join("\n")
      let a2 = stack.split("\n")
      let leResultate = (a1.length > a2.length ? a1 : a2)
        .map((l, i) => {
          return a1.length > a2.length
            ? l + (a2[i] || "")
            : (a1[i] || " ".repeat(30)) + l
        }).join("\n")
      return clc.xterm(255).bold(header) + "\n" + leResultate + "\n\n" + src
    },
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
      return format.formatDb(table, ["index", "deltaC", "success"], styles)
    },
    toggle: ["b", "B"]
  },
  debug: {
    view: (state) => {
      let debug = {
        frontier: state.behaviour.frontier
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
  log: {
    view: state => {
      return state.error || ""
    },
    toggle: ["l", "L"]
  }
}

module.exports = view
