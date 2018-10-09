const fs = require("fs");
const path = require("path");
const {
  get_pc_to_inst_map,
  genSrcmapArr
} = require("./srchandler.js");
const {
  testPath,
  revert,
  read
} = require("./util.js");

const toJson = str => JSON.parse(str);
const and = (a, b) => a && b;

module.exports = json => {

  // revert if no contracts field is given
  if(!json.implementations) revert("no implementations field in config");

  const srcs = {};
  const contracts = {};

  const control = {
    "0"      : "branch 0",
    "1"      : "branch 1",
    "2"      : "branch 2",
    "3"      : "branch 3",
    "4"      : "branch 4",
    "5"      : "branch 5",
    "6"      : "branch 6",
    "S-n"    : "next_k",
    "S-p"    : "prev_k",
    "n"      : "next_step",
    "p"      : "prev_step",
    "C-n"    : "next_branch",
    "C-p"    : "prev_branch",
    "e"      : "toggle_module evm",
    "b"      : "toggle_module behaviour",
    "c"      : "toggle_module constraint",
    "s"      : "toggle_module sourcemap",
    "r"      : "toggle_module rule",
    "k"      : "toggle_module kcell",
    "z"      : "toggle_module z3feedback",
    "d"      : "toggle_module debug",
    "up"     : "scroll_up",
    "down"   : "scroll_down"
  };

  json.control = {
    ...control,
    ...(json.control || {})
  }


  Object.keys(json.implementations)
    .forEach(alias => {
      // set contract name
      if(!json.implementations[alias].name) json.implementations[alias].name = alias;

      let c = toJson(read(json.implementations[alias].solc_output));

      c.sourceList
        .forEach(path => {
          srcs[path] = false
        });

      Object.keys(c.contracts)
        .forEach(k => {
          const name = k.split(":")[1]

          // if(name in cs) revert(name + " contract implemented multiple times");

          const bin_runtime = c.contracts[k]["bin-runtime"];
          const srcmap_runtime = c.contracts[k]["srcmap-runtime"];

          const {
            instructions,
            pc_to_inst_map
          } = get_pc_to_inst_map(bin_runtime)
          const inst_to_pc = instructions.map((inst, i) => pc_to_inst_map.indexOf(i));
          const srcmapArr = genSrcmapArr(srcmap_runtime);

          contracts[name] = {
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
    })

  json.contracts = contracts;

  const missingSources = Object.keys(srcs)
    .map(p => path.join(json.dapp_root, p))
    .filter(path => !testPath(path))
  if(missingSources.length > 0) revert("source file(s) not found: \n  " + missingSources.join("\n  "));

  json.srcs = Object.keys(srcs)
    .reduce((ss, p) => {
      ss[p] = read(path.join(json.dapp_root, p));
      return ss
    }, {})

  return json;
}
