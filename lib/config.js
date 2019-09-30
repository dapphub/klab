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

const main = (json) => {
  const dappout = `${json.dapp_root}/out/dapp.sol.json`
//  if (!testPath(dappout) && !bytecode) revert(`Dapp json not found: ${dappout}`)
  let dapp;
  if (testPath(dappout)) dapp = toJson(read(dappout))

  const contracts = {};
  Object.keys(json.implementations)
    .forEach(alias => {
      if(!json.implementations[alias].name) json.implementations[alias].name = alias;

      const name = json.implementations[alias].name
      if (contracts[name]) warn(`Multiple implementations found for ${name}`)

      let ast;
      let abi;
      let bin_runtime;
      let srcmap_runtime;
      let instructions;
      let pc_to_inst_map;
      let srcmapArr;
      let inst_to_pc;
      if (dapp) {
        const src = json.implementations[alias].src
        if (!(path.extname(src) == ".sol")) revert(`Non Solidity src for ${name}`)
        
        const fullname = `${src}:${name}`
        const contract = dapp.contracts[fullname]
        if (!contract) revert(`No contract named ${fullname} found in ${dappout}`)

        ast = dapp.sources[src].AST
        srcmap_runtime = contract["srcmap-runtime"];
        bin_runtime = contract["bin-runtime"];
        abi = contract["abi"];
        res = get_pc_to_inst_map(bin_runtime);
        instructions = res.instructions;
        pc_to_inst_map = res.pc_to_inst_map;
        inst_to_pc = instructions.map((inst, i) => pc_to_inst_map.indexOf(i));
        srcmapArr = genSrcmapArr(srcmap_runtime);
      } else {
        abi = json.implementations[name].abi ? fs.readFileSync(json.implementations[name].abi).toString() : abi
        bin_runtime = json.implementations[name].bin_runtime ? fs.readFileSync(json.implementations[name].bin_runtime) : bin_runtime
      }
      
      if (!ast)            warn(`Contract AST not found for ${name}`)
      if (!abi)            warn(`Contract abi not found for ${name}`)
      if (!bin_runtime)    warn(`Contract bin_runtime not found for ${name}`)
      if (!srcmap_runtime) warn(`Contract srcmap_runtime not found for ${name}`)

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
