const clc    = require("cli-color");
const fs     = require("fs");
const path   = require("path");
const locale = require("./locale.js");
const keccak = require("keccak");
const sha3 = function (str) {
  return keccak('keccak256')
    .update(str)
    .digest('hex')
    .toString()
}

const testPath = _path => {
  try {
    fs.accessSync(_path, fs.constants.F_OK);
    return true;
  } catch (e) {
    return false;
  }
}

const ensureDir  = dir => {
  if(!testPath(dir)) fs.mkdirSync(dir);
}

const ensureDirs = dirs => {
  dirs.forEach(ensureDir);
}

const revert = msg => console.error(clc.xterm(locale.color.ERROR)(msg)) || process.exit(1);
const read   = path => fs.readFileSync(path).toString();
const warn   = str => console.log(clc.xterm(214)(str))

module.exports = {
  ensureDirs,
  ensureDir,
  testPath,
  revert,
  read,
  sha3,
  warn
};
