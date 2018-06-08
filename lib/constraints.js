module.exports = {
  clean: cs => {
    return cs.split("\n")
      .map(s => s.replace(/\ #And$/, ""))
      .map(s => s.replace(/ ==K true/g,"").trim())
      .join("\n")
      .replace(/115792089237316195423570985008687907853269984665640564039457584007913129639936/g,"pow256")
      .replace(/1461501637330902918203684832716283019655932542976/g,"pow160")
      .replace(/\.\_\d\d+:\w+/g, " ")
      .replace(/Int/g,"")
      .replace(/==K/g,"==")
      .replace(/ mod /g, " % ")
      .replace(/  /g, " ")
  }
}
