const _ = require("lodash");
const clc = require('cli-color');
const { formatStep } = require("./formatter.js");
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
      const formatConstraint = (c) => c.split("\n")
        .join("")
        .split("#And")
        .map(s => "  " + s.replace(/ ==K true/g,"").trim())
        .join("\n")
        .replace(/115792089237316195423570985008687907853269984665640564039457584007913129639936/g,"pow256")
        .replace(/1461501637330902918203684832716283019655932542976/g,"pow160")
        .replace(/Int/g,"")
      constraint = formatConstraint(constraint);
      let old = formatConstraint(state.nodes[state.initt].constraint);
      old = old.split("\n").map(c => c.trim())
      constraint = constraint
        .split("\n")
        .map(c => c.trim())
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
      return getCodeStringFromPc(term.generatedTop.ethereum.evm.callState.pc)
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
  }
}

module.exports = {formatRule, show};
