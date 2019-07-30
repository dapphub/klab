const fs = require("fs");
const path = require("path");
const {
  get_pc_to_inst_map,
  genSrcmapArr,
  getContractAst
} = require("./srchandler.js");
const {
  testPath,
  revert,
  warn,
  read
} = require("./util.js");

const toJson = str => JSON.parse(str);

module.exports = (json) => {
  const dappout = `${json.dapp_root}/out/dapp.sol.json`
  if (!testPath(dappout)) revert(`Dapp json not found: ${dappout}`)
  const dapp = toJson(read(dappout))

  const contracts = {};
  Object.keys(json.implementations)
    .forEach(alias => {
      if(!json.implementations[alias].name) json.implementations[alias].name = alias;

      const name = json.implementations[alias].name
      if (contracts[name]) warn(`Multiple implementations found for ${name}`)

      const src = json.implementations[alias].src
      if (!(path.extname(src) == ".sol")) revert(`Non Solidity src for ${name}`)

      const fullname = `${src}:${name}`
      const contract = dapp.contracts[fullname]
      if (!contract) revert(`No contract named ${fullname} found in ${dappout}`)

      const ast = dapp.sources[src].AST
      const abi = contract["abi"]
      const bin_runtime = contract["bin-runtime"]
      const srcmap_runtime = contract["srcmap-runtime"]

      if (!ast)            warn(`Contract AST not found for ${fullname}`)
      if (!abi)            warn(`Contract abi not found for ${fullname}`)
      if (!bin_runtime)    warn(`Contract bin_runtime not found for ${fullname}`)
      if (!srcmap_runtime) warn(`Contract srcmap_runtime not found for ${fullname}`)

      const { instructions, pc_to_inst_map } = get_pc_to_inst_map(bin_runtime)
      const inst_to_pc = instructions.map((inst, i) => pc_to_inst_map.indexOf(i));
      const srcmapArr = genSrcmapArr(srcmap_runtime);

      if (!contracts[name]) {
        contracts[name] = {
          bin_runtime,
          srcmap_runtime,
          abi,
          srcs: dapp.sourceList,
          instructions,
          pc_to_inst_map,
          inst_to_pc,
          srcmapArr,
          ast
        }
      }
    })
  json.contracts = contracts;

  const missingSources = dapp.sourceList
    .map(src => path.join(json.dapp_root, src))
    .filter(p => !testPath(p))

  if(missingSources.length > 0) revert("source file(s) not found: \n  " + missingSources.join("\n  "));

  json.srcs = dapp.sourceList
    .reduce((ss, p) => {
      ss[p] = read(path.join(json.dapp_root, p));
      return ss
    }, {})

  return json;
}
