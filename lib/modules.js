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
    view: (state, settings) => {
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
    view: (state, settings) => {
      let id = state.path[state.path.length - 1].step.to.split("_")[1]
      let cs = K.formatConstraint(state.nodes[id])
      return Constraints.clean(cs);
    },
    toggle: ["c", "C"]
  },
  source: {
    view: (state, settings) => {
      let id = state.path[state.path.length - 1].step.to;
      let term = state.nodes[id].term;
      return getCodeStringFromPc(settings, term.generatedTop.ethereum.evm.callState.pc)
    },
    toggle: ["s", "S"]
  },
  rule: {
    view: (state, settings) => {
      let id = state.path[state.path.length - 1].step.rule;
      let rule = state && state.rules && state.rules[id] && formatRule(state.rules[id]) || "";
      return rule;
    },
    toggle: ["r", "R"]
  },
  behaviour: {
    view: (state, settings) => {
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
    view: (state, settings) => {
      let debug = {
        sid: state.sid
      };
      return JSON.stringify(debug, false, 2);
    },
    toggle: ["d"]
  },
  memory: {
    view: (state, settings) => {
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
    view: (state, settings) => {
      let nodeId = state.path[state.path.length - 1].step.to
      if (! (nodeId in state.z3feedback)) return ""
      let z3feedback = state.z3feedback[nodeId]
      let rhs        = K.formatConstraint(z3feedback.rhs)
      let z3QueryId  = z3feedback.queryId
      let z3Result   = z3feedback.result.term.token
      return "result: " + clc.xterm(210)(z3Result)   + "\n"
           + "queryId: " + z3QueryId                 + "\n"
           + "checking:"                             + "\n"
           + "    " + rhs.split("\n").join("\n    ") + "\n"
    },
    toggle: ["z", "Z"]
  }
}

module.exports = view
