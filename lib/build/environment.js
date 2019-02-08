const fs = require("fs");
const path = require("path");
const {
  genSrcmapArr
} = require("../srchandler.js");
const {
  testPath,
  revert,
  warn,
  read,
  get_pc_to_inst_map
} = require("../util/util.js");

const toJson = str => JSON.parse(str);
const and = (a, b) => a && b;

const KLAB_OUT = process.env.KLAB_OUT || "out";

module.exports = (json) => {

  // revert if no contracts field is given
  if(!json.implementations) revert("no implementations field in config");

  const srcs = {};
  const contracts = {};

    if (json.implementations) {
  Object.keys(json.implementations)
    .forEach(alias => {
      // set contract name
        if(!json.implementations[alias].name) json.implementations[alias].name = alias;
        const name = json.implementations[alias].name

      let bin_runtime;
      let srcmap_runtime;
      let _abi;
      let _srcs;
      let ast;
      if (testPath(json.implementations[alias].solc_output)) {
        let c = toJson(read(json.implementations[alias].solc_output));

        c.sourceList
            .forEach(path => {
                srcs[path] = false
            });
        let key = Object.keys(c.contracts).filter(k => k.split(':')[1] == name)
        if (key.length == 0) {revert(`No contract named ${name} found in ${json.implementations[alias].solc_output}`)}
        let sourceKey = c.sourceList.filter(k => k.split('/').slice(-1)[0] == json.implementations[alias].solc_output.replace('.json','').split('/').slice(-1)[0])
        if(!c.sources[sourceKey].AST) warn(`Contract AST not found for ${name}`)
        ast = c.sources[sourceKey].AST
        bin_runtime = c.contracts[key]["bin-runtime"];
        if(!bin_runtime) warn(`Contract bin_runtime not found for ${name}`)
        srcmap_runtime = c.contracts[key]["srcmap-runtime"];
        _abi = c.contracts[key].abi;
        _srcs = c.sourceList;
      }
      bin_runtime = json.implementations[alias].bin_runtime ? read(json.implementations[alias].bin_runtime) : bin_runtime;
        const {
           instructions,
           pc_to_inst_map
        } = get_pc_to_inst_map(bin_runtime)
      const inst_to_pc = instructions.map((inst, i) => pc_to_inst_map.indexOf(i));
      if(!srcmap_runtime) warn(`Contract srcmap_runtime not found for ${name}`)
      const srcmapArr = srcmap_runtime ? genSrcmapArr(srcmap_runtime) : undefined;

        if (!contracts[name]) {
            contracts[name] = {
                bin_runtime,
                srcmap_runtime,
                abi: _abi,
                srcs: _srcs,
                instructions,
                pc_to_inst_map,
                inst_to_pc,
                srcmapArr,
                ast
            }
        }
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
    }
  return json;
}
