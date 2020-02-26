const kast = require("./kast.js");

const getInfo = k => ({
  pc: kast.format( kast.get(k, "ethereum.evm.callState.pc") ),
  call_id: kast.format( kast.get(k, "ethereum.evm.callState.id") ),
  acc_map: kast.get(k, "ethereum.network.accounts"),
  stack_kast: kast.flattenNthByteOp(kast.get(k, "ethereum.evm.callState.wordStack"))
})

const buildDisplayInfo = k => {

  const {
    pc,
    call_id,
    acc_map,
    stack_kast
  } = getInfo(k);

  let kcell = kast.get(k, 'k');

  const reduceKSequence = term => {
    if(term.node == "KApply" && term.label == "#KSequence") {
      return [term.args[0]].concat(reduceKSequence(term.args[1]))
    } else {
      return [term];
    }
  }
  const findCodeDeposit = reduceKSequence(kcell)
    .find(term => term.node == "KApply" && term.label == "#codeDeposit__EVM")
  const codeDeposit = findCodeDeposit
    && kast.format(findCodeDeposit.args[0]);
  const isCreate = call_id == codeDeposit;

  let stack_flatt_kast = kast.flatten(stack_kast, "_:_WS")
    .filter(c => c.label !== ".WordStack_EVM-TYPES")
    .map(c => kast.format(c))


  let acc = acc_map
    .args.find(o => {
      return o.args
        && o.args[0]
        && o.args[0].args
        && o.args[0].args[0]
        && kast.format(o.args[0].args[0]) == call_id
    })
    .args[1]
    let storage_object = kast.get(acc, "storage");

  return {
    pc,
    stack_flatt_kast,
    storage_object,
    call_id,
    isCreate
  };
}

module.exports = {
  getInfo,
  buildDisplayInfo
}
