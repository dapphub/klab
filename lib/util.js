const shorten = (str, num) => {
  if(str.split("\n").length > num) {
    return str.split("\n").slice(0, num/2)
    .concat([clc.red("  [...]")])
      .concat(string.split("\n").slice(-num/2))
    .join("\n")
  }
  return str;
}

const indent = (str, num) => {
  return rule.string.split("\n").join("\n" + " ".repeat(num))
}

module.exports = {
  shorten
};
