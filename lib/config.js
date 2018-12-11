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
const and = (a, b) => a && b;

const KLAB_OUT = process.env.KLAB_OUT || "out";

module.exports = (json, k_path) => {

  // revert if no contracts field is given
  if(!json.implementations) revert("no implementations field in config");

  if(k_path) {
    json.spec = k_path && (
      testPath(k_path) && read(k_path)
      || revert(`spec not found at ${k_path}`))
      || revert("no <spec> or --name provided. Try `klab run out/specs/SOME_SPEC`");

    json.name = k_path && path.basename(k_path)
    json.rule_name = json.name.slice(0, -2);
    const hash_pointer_path = path.join(KLAB_OUT, "meta", "name", json.rule_name);
    json.rule_hash = testPath(hash_pointer_path)
      && read(hash_pointer_path)
      || revert(`no pointer found at ${hash_pointer_path}`)
    const meta_path = path.join(KLAB_OUT, "meta", "data", json.rule_hash);
    json.rule_meta = testPath(meta_path)
      && JSON.parse(read(meta_path))
      || revert(`no metadata found at ${json.rule_hash}`)
  }


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
    "g"      : "toggle_module gas",
    "m"      : "toggle_module memory",
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
        const name = json.implementations[alias].name

        let c = toJson(read(json.implementations[alias].solc_output));

        c.sourceList
            .forEach(path => {
                srcs[path] = false
            });

        /*
      Object.keys(c.contracts)
        .forEach(k => {
            const name = k.split(":")[1]
            const contractAst = getContractAst([ast], name)[0]
            //console.log('contractAst')
            //console.log(contractAst)
         /*   let ast;
            if (json.implementations[alias].ast) {
                console.log('we got an ast for ' + alias)
                ast = toJson(read(json.implementations[alias].ast));
                //            console.log(ast)
            }
        */

        // if(name in cs) revert(name + " contract implemented multiple times");
        let key = Object.keys(c.contracts).filter(k => k.split(':')[1] == name)
        if (key.length == 0) {revert(`No contract named ${name} found in ${json.implementations[alias].solc_output}`)}
        let sourceKey = c.sourceList.filter(k => k.split('/').slice(-1)[0] == json.implementations[alias].solc_output.replace('.json','').split('/').slice(-1)[0])
        let ast = c.sources[sourceKey].AST
        if(!ast) warn(`Contract AST not found for ${name}`)
        const bin_runtime = c.contracts[key]["bin-runtime"];
        if(!bin_runtime) revert(`Contract bin_runtime not found for ${name}`)
        const srcmap_runtime = c.contracts[key]["srcmap-runtime"];
        if(!srcmap_runtime) revert(`Contract srcmap_runtime not found for ${name}`)
        const {
           instructions,
           pc_to_inst_map
        } = get_pc_to_inst_map(bin_runtime)
        const inst_to_pc = instructions.map((inst, i) => pc_to_inst_map.indexOf(i));
        const srcmapArr = genSrcmapArr(srcmap_runtime);
        if (!contracts[name]) {
            contracts[name] = {
                bin_runtime,
                srcmap_runtime,
                abi: c.contracts[key].abi,
                srcs: c.sourceList,
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

  return json;
}
