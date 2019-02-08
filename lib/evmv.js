const kast = require("./util/kast.js");
const {
  buildDisplayInfo
} = require("../lib/compile.js");
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

  const {
    stack_flatt_kast,
    pc,
    storage_object,
    call_id
  } = buildDisplayInfo(k, state.config);


  //Extract bin_runtime from program cell
  // console.log(bin_runtime);
  let spec_o = state.config.rule_meta
  let contract_o = state.config.implementations[spec_o.v2n[call_id]];
  let contract = state.config.contracts[contract_o.name];
  // TODO - use var to contractname map to get the contractname
  //        and use it to get the right contract here

  // STORAGE
  let storage_str = storage_object;
//  let storage_str = JSON.stringify(storage_object, false, 2)

  // PC + OPs
  let inst = contract.pc_to_inst_map[pc];
  let from = Math.max(0, inst - 5);
  let to   = Math.min(contract.instructions.length, from + 10);
  let pc_op_str = contract.instructions
    .slice(from, to)
    .map((s, i) => (from + i === inst ? "> " : "  ") + r(hex(contract.inst_to_pc[from + i])) + "  " + r(hex(from + i)) + "  " + (s.length > 20 ? s.slice(0, 20) + "..." : s))
    .map((s,i) => clc.xterm(from + i === inst ? show_color : hide_color)(s))
    .join("\n")

  // STACK
  // const stack_kast = kast.flattenNthByteOp(kast
  //   .get(k, "ethereum.evm.callState.wordStack"))

  const tw = process.stdout.columns; // total width
  const stack_str = stack_flatt_kast
    .map((v, i) => {
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

  let header = clc.xterm(255).bold("    PC    ID  Opcode" + " ".repeat(22) + "Stack\n");

  let leResultate = linejoin(pc_op_str, stack_str, 42);

  return header
    + leResultate
    + "\n\nSTORAGE\n"
    + storage_str
}
