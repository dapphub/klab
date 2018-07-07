const kast = require("./kast.js");
const {
  getCodeStringFromPc,
} = require("../lib/srchandler.js");
const r = s => " ".repeat(4 - s.length) + s
const hex = n => n.toString(16).length % 2 == 0 ? n.toString(16) : "0" + n.toString(16);
const clc = require('cli-color');
const show_color = 255;
const hide_color = 244;
const S = require("./state.js");
const Constraints = require("./constraints.js");

module.exports = function (state) {
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
}
