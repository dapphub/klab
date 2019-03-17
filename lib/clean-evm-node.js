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

  let stack_flatt_kast = kast.flatten(stack_kast, "_:__EVM-DATA")
    .filter(c => c.label !== ".WordStack_EVM-DATA")
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
    let storage_object = kast.format(kast.get(acc, "storage"));

  return {
    pc,
    stack_flatt_kast,
    storage_object,
    call_id
  };
}

module.exports = {
  getInfo,
  buildDisplayInfo
}
