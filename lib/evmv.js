const kast = require("./kast.js");
const {
  getCodeStringFromPc,
} = require("../lib/srchandler.js");
const r = s => " ".repeat(4 - s.length) + s
const hex = n => n.toString(16).length % 2 == 0 ? n.toString(16) : "0" + n.toString(16);
const try_hex = _s => {
  if(/^\d*$/.test(_s.trim())) {
    let n = new BN(_s, 10);
    s = n.toString(16)
    s = "0x" + (s.length % 2 == 1 ? "0" : "") + s;
    return s;
  } else {
    return _s.trim()
  }
}
const clc = require('cli-color');
const show_color = 255;
const hide_color = 244;
const Constraints = require("./constraints.js");
const S = require("./state.js");
const BN = require('bn.js');

const linejoin = (_a, _b, min_distance = 30) => {
  let a = _a.split("\n")
  let b = _b.split("\n")
  return (a.length > b.length ? a : b)
    .map((l, i) => {
      let space = " ".repeat(Math.max(0, min_distance - clc.getStrippedLength(l)));
      return a.length > b.length
        ? l + space + (b[i] || "")
        : (b[i] || " ".repeat(30)) + l
    }).join("\n")
}

module.exports = function (state) {
  let id = S.term_id(state)
  let k = state.nodes[id].term;
  let pc = kast.get(k, "ethereum.evm.callState.pc");
  let src = getCodeStringFromPc(state.config, parseInt(pc), true, state.config.pc_to_inst_map, state.config.srcmapArr);
  let inst = state.config.pc_to_inst_map[pc];
  let from = Math.max(0, inst - 5);
  let to   = Math.min(state.config.instructions.length, from + 10);
  let header = ("    PC    ID  Opcode" + " ".repeat(22) + "Stack");

  // STORAGE
  let call_id = kast.get(k, "ethereum.evm.callState.id")
  let acc_map = kast.get(k, "ethereum.network.accounts")
  let flatten = map => map.label == "_Map_"
    ? [map.args[0]].concat(flatten(map.args[1]))
    : [map]
  let acc = acc_map
    .args.find(o => o.args[0].args[0].name == call_id)
    .args[1]
  let storage = kast.get(acc, "storage")
  let flat_storage = flatten(storage)
  let store = kast.foldMap(flat_storage)
  let storage_str = Object.keys(store)
    .map(k => k + " ".repeat(Math.max(0, 10 - k.length)) + store[k])
    .join("\n")
    // .reduce((str, k) => str + "\n" + , "")

  // PC + OPs
  let pc_op_str = state.config.instructions
    .slice(from, to)
    .map((s, i) => (from + i === inst ? "> " : "  ") + r(hex(state.config.inst_to_pc[from + i])) + "  " + r(hex(from + i)) + "  " + (s.length > 20 ? s.slice(0, 20) + "..." : s))
    .map((s,i) => clc.xterm(from + i === inst ? show_color : hide_color)(s))
    .join("\n")

  // STACK
  let stack = kast
    .get(k, "ethereum.evm.callState.wordStack")
    .split("\n")
  let stackArr = Constraints.clean(stack)
    .split(":")
    .map((s, i) => hex(i) + "  " + try_hex(s) )
    .slice(0, -1)
  stack = ( stackArr.length > 10 ? stackArr.slice(0, 9).concat(["..."]) : stackArr ).join("\n")

  let leResultate = linejoin(pc_op_str, stack, 42);

  return clc.xterm(255).bold(header) + "\n" + leResultate + "\n\n" + src + "\n\nSTORAGE\n" + storage_str
}
