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
        : (i in a && a[i] + " ".repeat(min_distance - clc.getStrippedLength(a[i])) || " ".repeat(min_distance)) + l
    }).join("\n")
}

module.exports = function (state) {
  let id = S.term_id(state)
  let k = state.nodes[id].term;
  let pc = kast.format( kast.get(k, "ethereum.evm.callState.pc") );


  // SOURCECODE
  let src = getCodeStringFromPc(state.config, parseInt(pc), true, state.config.pc_to_inst_map, state.config.srcmapArr);

  // STORAGE
  let call_id = kast.format( kast.get(k, "ethereum.evm.callState.id") )
  let acc_map = kast.get(k, "ethereum.network.accounts")
  let flatten = map => map.label == "_Map_"
    ? [map.args[0]].concat(flatten(map.args[1]))
    : [map]
  let acc = acc_map
    .args.find(o => o.args[0].args[0].name == call_id)
    .args[1]
  let storage = kast.flattenNthByteOp(kast.get(acc, "storage"))
  let storage_object = (storage.args || [])
    .filter(m => m.args)
    .map(m => [kast.format(m.args[0]), kast.format(m.args[1])])
    .reduce((a, [k, v]) => ({...a, [k]: v}), {})
  //JSON.stringify(storage, false, 2)
  let storage_str = JSON.stringify(storage_object, false, 2)

  // PC + OPs
  let inst = state.config.pc_to_inst_map[pc];
  let from = Math.max(0, inst - 5);
  let to   = Math.min(state.config.instructions.length, from + 10);
  let pc_op_str = state.config.instructions
    .slice(from, to)
    .map((s, i) => (from + i === inst ? "> " : "  ") + r(hex(state.config.inst_to_pc[from + i])) + "  " + r(hex(from + i)) + "  " + (s.length > 20 ? s.slice(0, 20) + "..." : s))
    .map((s,i) => clc.xterm(from + i === inst ? show_color : hide_color)(s))
    .join("\n")

  const tw = process.stdout.columns; // total width

  // STACK
  let stack_kast = kast
    .get(k, "ethereum.evm.callState.wordStack")
  let stack_flatt_kast = kast.flatten(stack_kast, "_:__EVM-DATA")
  let stack_str = stack_flatt_kast
    .filter(c => c.label !== ".WordStack_EVM-DATA")
    .map((c, i) => {
      let v = kast.format(c)
      let l = tw - 46;
      if(clc.getStrippedLength(v) > l) {
        v = v
          .match(new RegExp(`.{1,${ l - 1 }}`, "g"))
          .join("\n" + " ".repeat(4))
      }
      return hex(i)
        + "  "
        + v;
    })
    .join("\n")

  let header = ("    PC    ID  Opcode" + " ".repeat(22) + "Stack");

  let leResultate = linejoin(pc_op_str, stack_str, 42);

  return clc.xterm(255).bold(header) + "\n" + leResultate + "\n\n" + src + "\n\nSTORAGE\n" + storage_str
}
