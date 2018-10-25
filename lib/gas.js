const { genBehaviour } = require("./behavior.js");

module.exports = state => {
  const behavior = genBehaviour(state);
  const clean = str => str.replace(/VGas\ \-Int\ /g, "")
  const getGasExpr = k => k.children.length == 0
    && (k.gas && clean(k.gas) || `<${JSON.stringify(k, false, 2)}>`)
    || `#if (${k.children[0].deltaCRaw.trim()}) #then (${getGasExpr(k.children[0])}) #else (${getGasExpr(k.children[1])}) #fi`

  return getGasExpr(behavior);
}
