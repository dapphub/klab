const _ = require("lodash");
const clc = require('cli-color');
const { formatStep } = require("./formatter.js");
const { formatMemory } = require("./unwrapper.js");
const { getCodeStringFromPc } = require("../lib/srchandler.js");
const genBehaviour = require("./helper.js");
const Constraints = require("./constraints.js");


const formatRule = rule => {
  if(!rule) return "?"
  let string = clc.xterm(0)(`${rule.filepath} ${rule.from}-${rule.to}\n     `) + rule.string.split("\n").join("\n  ").trim();
  if(string.split("\n").length > 6) {
    string = string.split("\n").slice(0, 3)
    .concat([clc.red("  [...]")])
    .concat(string.split("\n").slice(-3))
    .join("\n")
  }
  return string;
}

const show = {
  term: {
    exec: (state, settings) => {
      let id = state.path[state.path.length - 1].step.to.split("_")[0];
      let term = state.nodes[id].term;
      return JSON.stringify(term, false, 2);
    },
    toggle: ["t", "T"]
  },
  constraint: {
    exec: (state, settings) => {
      let id = state.path[state.path.length - 1].step.to.split("_")[1];
      let constraint = state.nodes[id].term;
      return JSON.stringify(constraint, false, 2);
    },
    toggle: ["c", "C"]
  },
  source: {
    exec: (state, settings) => {
      let id = state.path[state.path.length - 1].step.to;
      let term = state.nodes[id].term;
      return getCodeStringFromPc(settings, term.generatedTop.ethereum.evm.callState.pc)
    },
    toggle: ["s", "S"]
  },
  rule: {
    exec: (state, settings) => {
      let id = state.path[state.path.length - 1].step.rule;
      let rule = state && state.rules && state.rules[id] && formatRule(state.rules[id]) || "";
      return rule;
    },
    toggle: ["r", "R"]
  },
  behaviour: {
    exec: (state, settings) => {
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
    exec: (state, settings) => {
      let debug = {
        sid: state.sid
      };
      return JSON.stringify(debug, false, 2);
    },
    toggle: ["d"]
  },
  memory: {
    exec: (state, settings) => {
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
    exec: (state, settings) => {
      let id = state.path[state.path.length - 1].step.to;
      let keys = Object.keys(state.z3feedback[id] || {});
      let display = keys
        .map(key => Object.assign({key}, state.z3feedback[id][key]))
        .map(o => {
          let result = o.result == "unknown"
                     ? clc.red(o.result)
                     : o.result;
          return o.key + " " + result + "\n  " +
            Constraints.clean(o.right).split("\n").join("\n  ")
        })
        .join("\n")
      return display;
    },
    toggle: ["z", "Z"]
  }
}

module.exports = {formatRule, show};
