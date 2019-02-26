const { genBehaviour } = require("./behavior.js");

module.exports = state => {
  const { behaviour } = genBehaviour(state);
  // const clean = str => str.replace(/VGas\ \-Int\ /g, "")
  const if_else_fi = args => ({
    node: "KApply",
    label: "#if_#then_#else_#fi_K-EQUAL",
    variable: false,
    arity: 3,
    args: args
  })
  const getGasExpr = k => k.children.length == 0
    && k.gas
    || k.children.length > 1 && if_else_fi([
      k.children[0].deltaCRaw,
      getGasExpr(k.children[0]),
      getGasExpr(k.children[1])
    ])

  return JSON.stringify(getGasExpr(behaviour));
}
