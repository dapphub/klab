const clc = require('cli-color');
const fs = require("fs");

const getFileExcerpt = (path, from, to) => {
  try {
    return fs
    .readFileSync(path)
    .toString()
    .split("\n")
    .slice(from - 1, to)
    .filter(l => l != "")
    .join("\n");
  } catch(e) {
    return " ??? ";
  }
}

// TODO - server
const parseRule = ruleString => {
  const pos_regex = /Location\((\d+)\,\d+\,(\d+)\,\d+\)/;
  const src_regex = /Source\(Source\(([^\)]+)\)/;
  const location = ruleString.match(pos_regex);
  const filepath = ruleString.match(src_regex)[1];
  const from = location[1];
  const to = location[2];
  // TODO - assert filepath exist
  let string = getFileExcerpt(filepath, parseInt(from), parseInt(to)).trim()

  return {
    from,
    to,
    filepath,
    string
  };
}

const shorten = (str, num) => {
  if(str.split("\n").length > num) {
    return str.split("\n").slice(0, num/2 + 1)
    .concat([clc.red("  [...]")])
      .concat(str.split("\n").slice(-num/2))
    .join("\n")
  }
  return str;
}

const formatRule = rule => {
  if(!rule) return "?"
  let string = clc.xterm(0)(`${rule.filepath} ${rule.from}-${rule.to}\n     `) + rule.string.split("\n").join("\n" + " ".repeat(2))
  string = shorten(string, 5)
  return string;
}

module.exports = {
  parseRule,
  formatRule
}
