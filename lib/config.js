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
  if (!("solc_output_path" in json)) json.solc_output_path = "out/dapp.sol.json"
  const dappout = `${json.dapp_root}/${json.solc_output_path}`
  if (!testPath(dappout)) revert(`solc json output not found: ${dappout}`)
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
      const abi = typeof(contract["abi"]) === "string" ? JSON.parse(contract["abi"]) : contract["abi"]
      const bin = contract["bin"]
      const bin_runtime = contract["bin-runtime"]
      const srcmap_runtime = contract["srcmap-runtime"]
      const srcmap = contract["srcmap"]

      if (!ast)            warn(`Contract AST not found for ${fullname}`)
      if (!abi)            warn(`Contract abi not found for ${fullname}`)
      if (!bin)            warn(`Contract bin not found for ${fullname}`)
      if (!bin_runtime)    warn(`Contract bin_runtime not found for ${fullname}`)
      if (!srcmap_runtime) warn(`Contract srcmap_runtime not found for ${fullname}`)
      if (!srcmap)         warn(`Contract srcmap not found for ${fullname}`)

      const bin_instructions_meta = get_pc_to_inst_map(bin)
      const bin_instructions = bin_instructions_meta.instructions;
      const bin_pc_to_inst_map = bin_instructions_meta.pc_to_inst_map;
      const bin_runtime_instructions_meta = get_pc_to_inst_map(bin_runtime)
      const bin_runtime_instructions = bin_runtime_instructions_meta.instructions;
      const bin_runtime_pc_to_inst_map = bin_runtime_instructions_meta.pc_to_inst_map;
      const bin_inst_to_pc = bin_instructions.map((inst, i) => bin_pc_to_inst_map.indexOf(i));
      const bin_runtime_inst_to_pc = bin_runtime_instructions.map((inst, i) => bin_runtime_pc_to_inst_map.indexOf(i));
      const bin_runtime_srcmapArr = genSrcmapArr(srcmap_runtime);
      const bin_srcmapArr = genSrcmapArr(srcmap);

      if (!contracts[name]) {
        contracts[name] = {
          bin,
          bin_runtime,
          srcmap,
          srcmap_runtime,
          abi,
          srcs: dapp.sourceList,
          bin_instructions,
          bin_runtime_instructions,
          bin_pc_to_inst_map,
          bin_runtime_pc_to_inst_map,
          bin_inst_to_pc,
          bin_runtime_inst_to_pc,
          bin_srcmapArr,
          bin_runtime_srcmapArr,
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
