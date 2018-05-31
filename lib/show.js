const _ = require("lodash");
const clc = require('cli-color');
const { formatStep } = require("./formatter.js");
const { formatMemory } = require("./unwrapper.js");
const { getCodeStringFromPc } = require("../lib/srchandler.js");
const genBehaviour = require("./helper.js");


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
      let id = state.path[state.path.length - 1].step.to;
      let term = state.nodes[id].term;
      let term_ = _.omit(term, settings.omit).generatedTop;
      term_ = JSON.parse(JSON.stringify(term_));
      return formatStep(term_, {});
    },
    toggle: ["t", "T"]
  },
  constraint: {
    exec: (state, settings) => {
      let id = state.path[state.path.length - 1].step.to;
      let constraint = state.nodes[id].constraint;
      let old = state.nodes[state.initt].constraint;
      constraint = constraint
        .split("\n")
        .map(c => old.indexOf(c) > -1 ? c : clc.xterm(215)(c))
        .join("\n")
      return constraint;
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
  }
}

module.exports = {formatRule, show};
