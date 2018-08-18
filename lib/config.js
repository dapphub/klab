const fs = require("fs");
const path = require("path");
const {
  get_pc_to_inst_map,
  genSrcmapArr
} = require("./srchandler.js");
const testPath = path => {
  try {
    fs.accessSync(path, fs.constants.F_OK);
    return true;
  } catch (e) {
    return false;
  }
}

const revert = msg => console.log(msg) || process.exit(1);
const read = path => fs.readFileSync(path).toString();
const toJson = str => JSON.parse(str);
const and = (a, b) => a && b;

module.exports = json => {

  // revert if no implements field is given
  if(!json.implements) revert("no implements field in config and no --implements given");

  // revert if wrong paths are given
  if(!json.implements.map(testPath).reduce(and, true)) revert("implements path not found");

  const srcs = {};


  const contracts = json.implements
    .map(read)
    .map(toJson)
    .reduce((cs, c) => {
      c.sourceList
        .forEach(path => {
          srcs[path] = false
        });

      Object.keys(c.contracts)
        .forEach(k => {
          const name = k.split(":")[1]

          if(name in cs) revert(name + " contract implemented multiple times");

          const bin_runtime = c.contracts[k]["bin-runtime"];
          const srcmap_runtime = c.contracts[k]["srcmap-runtime"];

          const {
            instructions,
            pc_to_inst_map
          } = get_pc_to_inst_map(bin_runtime)
          const inst_to_pc = instructions.map((inst, i) => pc_to_inst_map.indexOf(i));
          const srcmapArr = genSrcmapArr(srcmap_runtime);

          cs[name] = {
            bin_runtime,
            srcmap_runtime,
            abi: c.contracts[k].abi,
            srcs: c.sourceList,
            instructions,
            pc_to_inst_map,
            inst_to_pc,
            srcmapArr
          }
        })
      return cs;
    }, {})

  json.contracts = contracts;

  // TODO - check if behaviour_of has an implementation
  if(!(json.behaviour_of in json.contracts)) revert("implementation of " + json.behaviour_of + " is not given")

  const isSrcPresent = Object.keys(srcs)
    .map(p => path.join(json.path_root, p))
    .map(testPath)
    .reduce(and, true)
  if(!isSrcPresent) revert("source file not found");

  json.srcs = Object.keys(srcs)
    .reduce((ss, p) => {
      ss[p] = read(path.join(json.path_root, p));
      return ss
    }, {})

  // TODO - gen vars to contractname map
  json.var2name = {
    "ACCT_ID": json.behaviour_of,
    ...json.var2name
  }

  return json;
}
