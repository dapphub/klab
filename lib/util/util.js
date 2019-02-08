const clc    = require("cli-color");
const fs     = require("fs");
const path   = require("path");
const locale = require("./locale.js");
const keccak = require("keccak");
// EVM Instructions
const evm_i = require("../../resources/evm.json");
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


// HELPER
const hex = i => (s = i.toString(16)).length == 1 ? "0" + s : s;


// Instructions
const get_pc_to_inst_map = bin_string => {
  const code_arr = []
  for(let i=0; i<bin_string.length; i+=2) {
    code_arr.push(parseInt(bin_string.slice(i, i+2), 16));
  }
  const pc_to_inst_map = [];
  const instructions = [];
  let i = 0;
  while(i < code_arr.length) {
    let i_ = i;
    let index = hex(code_arr[i]);
    let instruction_string = evm_i[index] || `UNKNOWN (${index})`;

    if(code_arr[i] >= 96 && code_arr[i] <= 127) {
      let length = code_arr[i] - 95;
      instruction_string += " " + code_arr.slice(i + 1, i + 1 + length).map(s => hex(s)).join("");
      i += length;
    }
    i++;
    instructions.push(instruction_string);
    for(let j=i_; j < i; j++) {
      pc_to_inst_map.push(instructions.length - 1);
    }
  }
  return {
    pc_to_inst_map,
    instructions
  };
}

let getNodeSrcAst = (ast, name) => {
    return ast.map(node => {
        if (node.attributes && node.attributes.name) {
            if (node.attributes.name == name) {
                return node.src;
            }
        }
        if (node.children) {
            return getNodeSrcAst(node.children, name).join('');
        }
    })
}

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
  getId,
  get_pc_to_inst_map,
  getNodeSrcAst
};
