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

const main = (json, bytecode = undefined) => {
  const dappout = `${json.dapp_root}/out/dapp.sol.json`
  if (!testPath(dappout) && !bytecode) revert(`Dapp json not found: ${dappout}`)
  let dapp;
  if (testPath(dappout)) dapp = toJson(read(dappout))

  const contracts = {};
  Object.keys(json.implementations)
    .forEach(alias => {
      if(!json.implementations[alias].name) json.implementations[alias].name = alias;

      const name = json.implementations[alias].name
      if (contracts[name]) warn(`Multiple implementations found for ${name}`)

      const src = json.implementations[alias].src
      if (!(path.extname(src) == ".sol")) revert(`Non Solidity src for ${name}`)

      const fullname = `${src}:${name}`
      let ast;
      let abi;
      let bin_runtime;
      let srcmap_runtime;
      let instructions;
      let pc_to_inst_map;
      let srcmapArr;
      let inst_to_pc;
      if (dapp) {
        const contract = dapp.contracts[fullname]
        if (!contract) revert(`No contract named ${fullname} found in ${dappout}`)

        ast = dapp.sources[src].AST
        abi = contract["abi"]
        bin_runtime = contract["bin-runtime"]
        srcmap_runtime = contract["srcmap-runtime"]
        
        (instructions, pc_to_inst_map) = get_pc_to_inst_map(bin_runtime)
        inst_to_pc = instructions.map((inst, i) => pc_to_inst_map.indexOf(i));
        srcmapArr = genSrcmapArr(srcmap_runtime);
      }
      else if (bytecode) {
        bin_runtime = fs.readFileSync(bytecode).toString().replace('0x','')
      }
      if (!ast)            warn(`Contract AST not found for ${fullname}`)
      if (!abi)            warn(`Contract abi not found for ${fullname}`)
      if (!bin_runtime)    warn(`Contract bin_runtime not found for ${fullname}`)
      if (!srcmap_runtime) warn(`Contract srcmap_runtime not found for ${fullname}`)

      if (!contracts[name]) {
        contracts[name] = {
          bin_runtime,
          srcmap_runtime,
          abi,
          instructions,
          pc_to_inst_map,
          inst_to_pc,
          srcmapArr,
          ast
        }
      }
      if (dapp) contracts[name].srcs = dapp.sourceList
    })
  json.contracts = contracts;

  if (dapp) {
  const missingSources = dapp.sourceList
    .map(src => path.join(json.dapp_root, src))
    .filter(p => !testPath(p))

  if(missingSources.length > 0) revert("source file(s) not found: \n  " + missingSources.join("\n  "));

  json.srcs = dapp.sourceList
    .reduce((ss, p) => {
      ss[p] = read(path.join(json.dapp_root, p));
      return ss
    }, {})
  }
  return json;
}

module.exports = {main}
