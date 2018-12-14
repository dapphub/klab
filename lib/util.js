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
const KLAB_OUT     = process.env.KLAB_OUT || "out";

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

const revert = msg  => console.error(clc.xterm(locale.color.ERROR)(msg)) || process.exit(1);
const read   = path => fs.readFileSync(path).toString();
const warn   = str  => process.stderr.write(clc.xterm(214)(str) +"\n")

const render = (str, obj) => str.replace(/\{\{([^\}]*)\}\}/g, (_, match) => obj[match] || "")

const getId = spec_path => {

  const spec = read(spec_path);

  const rules = testPath(path.join(KLAB_OUT, "rules.k")) && read(path.join(KLAB_OUT, "rules.k"))
    || revert(`no rules found at ${KLAB_OUT}/rules.k`)

  const smt_prelude = testPath(path.join(KLAB_OUT, "prelude.smt2")) && read(path.join(KLAB_OUT, "prelude.smt2"))
    || revert(`no smt prelude file at ${KLAB_OUT}/prelude.mst2`)

  const proofid = sha3(JSON.stringify({
    rules: rules,
    spec : spec,
    smt_prelude: smt_prelude
  }));

  return proofid;
}

//TODO: refactor and get rid of these repetitions
const mapInterface = (is, str) => {
  is.forEach(([t, n]) => {
    str = str.replace(new RegExp("([^\\w\.]|^)" + n + "([^\\w]|$)", "g"), (a, b, c, d) => {
      return b + "ABI_" + n + c;
    })
  })
  return str;
}

const toK = str => str
  .replace(/ \+ /g, " +Int ")
  .replace(/ \- /g, " -Int ")
  .replace(/ \* /g, " *Int ")
  .replace(/ \/ /g, " /Int ")
  .replace(/ \> /g, " >Int ")
  .replace(/ \< /g, " <Int ")
  .replace(/ \<\= /g, " <=Int ")
  .replace(/ \>\= /g, " >=Int ")
  .replace(/ \=\= /g, " ==Int ")
  .replace(/ \=\/\= /g, " =/=Int ")
  .replace(/ and /g, " andBool ")
  .replace(/ or /g, " orBool ")
  .replace(/ not /g, " notBool ")
  .replace(/uint\(/g, "#unsigned(")
  .replace(/bool\(/g, "bool2Word(")


module.exports = {
  ensureDirs,
  ensureDir,
  testPath,
  render,
  revert,
  read,
  sha3,
  warn,
  mapInterface,
  toK,
  getId
};
