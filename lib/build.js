// TODO migrate prelude rules export from klab build into here and make sure make driver does it the same way.
// make sure the new structure works with server/client
const fs            = require("fs");
const path          = require("path");
const marked        = require("marked");
const kjson         = require("../lib/kjson.js");
const kast          = require("../lib/kast.js");
const srchandler    = require("./srchandler.js");
const {
  testPath,
  read,
  revert,
  warn,
  mapInterface,
  toK
}                   = require("./util.js")
const {collisionCheck} = require("./storage.js");
const KLAB_OUT      = process.env.KLAB_OUT || "out";
const omit          = require("lodash").omit;

const STORAGE_DELIMITER = "\n" + " ".repeat(12);
const IFF_DELIMITER      = "";
const brackedJoin = (delimiter, force) => (a, str, i) => (i > 0 || force ? delimiter : "") + `(${str + a})`

const getActs = str => {
  const raw_config = marked.lexer(str)

  const acts_str = raw_config
    .filter(e => e.type === "code" && e.lang == "act")
    .map(e => e.text)
    .join("\n\n")

  const acts_str_arr  = acts_str
    .split(/\nbehaviour\s/)
    .map((s, i) => (i == 0 ? s : "behaviour " + s))

  return acts_str_arr;
}

const makePrelude = config => {
  const prelude_path = config.src.smt_prelude || "./src/prelude.smt2.md";
  if(!testPath(prelude_path)) {
    console.error(`ERR: smt prelude file not found at ${prelude_path}`);
    process.exit();
  }

  let smt_prelude = marked.lexer(read(prelude_path))
    .filter(e => e.type === "code")
    .map(e => e.text)
    .join("\n") + "\n"

  const prelude_out_path = path.join(KLAB_OUT, "prelude.smt2");
  let smt_prelude_old = ""
  if(testPath(prelude_out_path)) smt_prelude_old = read(prelude_out_path);

  return {
    write_prelude: smt_prelude_old != smt_prelude,
    prelude_str: smt_prelude
  };
}

const makeRules = config => {

const rules_template = rules => `requires "edsl.k"

module RULES
    imports EVM
    imports EDSL
    imports K-REFLECTION

${rules.join("\n\n")}

endmodule
`

  const rule_strs = config.src.rules
    .map(read)
    .map((rule_str, i) => "```\n// " + config.src.rules[i] + "\n\n```\n" + rule_str)
    .join("\n")

  const rules = marked.lexer(rule_strs)
    .filter(e => e.type === "code")
    .map(e => e.text)
    .join("\n")

  const rules_k = rules_template([
    fs.readFileSync(path.join(__dirname, "../resources/rules.k.tmp")).toString(),
    rules
  ])
  const rules_path = path.join(KLAB_OUT, "rules.k");
  let rules_k_old = "";
  if(testPath(rules_path)) rules_k_old = read(rules_path);
  return {
    rules_str: rules_k,
    write_rules: rules_k_old != rules_k
  }
}

const longForm = str => {
    if (str == 'uint') {
        return 'uint256';
    } else if (str == 'int') {
        return 'int256';
    } else {
        return str;
    }
}

// populate uint_bounds and int_bounds
const int_sizes = Array.from(Array(32).keys()).map(n => n * 8 + 8)
const uint_keys = int_sizes.map(n => "uint" + n.toString())
const uint_vals = int_sizes.map(n => (name => `#rangeUInt(${ n }, ${ name })`))
const uint_bounds = Object.assign(...uint_keys.map((k, i) => ({[k]: uint_vals[i]})))
const int_keys = int_sizes.map(n => "int" + n.toString())
const int_vals = int_sizes.map(n => (name => `#rangeSInt(${ n }, ${ name })`))
const int_bounds = Object.assign(...int_keys.map((k, i) => ({[k]: int_vals[i]})))

const bound = Object.assign(uint_bounds, int_bounds, {
  "bool"   : name => `#rangeUInt(1, ${ name })`,
  "bytes32": name => `#rangeBytes(32, ${ name })`,
  "address": name => `#rangeAddress(${ name })`
})

const make_abi_dsl = ({type, name}, i) =>
  "#" + type + "(ABI_" + name + ")"
const make_args = inputs => inputs && inputs.length == 0
    && ".TypedArgs"
    || inputs.map(make_abi_dsl).join(", ")

const parseAct = config => (act_str, silent = false) => {
  let _act = act_str
    .split("\n")
    .reduce(([c, a], l) => {
      if(/^[^\s]/.test(l)) {
        if(c.length > 0) a = a.concat([c])
        c = [l]
      } else {
        c = c.concat([l])
      }
      return [c, a];
    }, [[], []])
  let act_arr = _act[1].concat([_act[0]])
  let act = act_arr
    .reduce((a, e) => {
      let head = e[0];
      let tail = e
        .slice(1)
        .filter(s => !(/^\s*\/\//.test(s)))
      if(/^behaviour\s/.test(head)) {
        a.name    = head.split(" ")[1]
        a.subject = head.split(" ")[3]
        a.varname2alias = {
          "ACCT_ID": a.subject
        }
        a.act_name = a.subject + "_" + a.name + ".act";
        if(config.DEBUG) console.log("parse " + a.subject + "." + a.name)
        fs.writeFileSync(path.join(KLAB_OUT, "acts", a.act_name), act_str)
      } else if (/^lemma/.test(head)) {
        a.internal = true;
        a.lemma = true;
      } else if(/^balance/.test(head)) {
        const balance_subject = head.split(" ")[1] || "ACCT_ID"
        const balance_object  = tail
          .map(l => l.trim())
          .filter(l => !!l)
        if(balance_object.length > 1) warn(`Balance of ${balance_subject} expected to be one statement, but ${balance_object.length} lines found!`)
        a.balance = {
          ...(a.balance || {}),
          [balance_subject]: toK(balance_object.join(" "))
        }
      } else if(/^interface/.test(head)) {
        // TODO - only parse - construct object somewhere else
        let interface_str = head
          .split(" ")
          .slice(1)
          .join(" ")
        a.internal = interface_str.split(' ').slice(-1) == 'internal' ? true : false;
        interface_str = interface_str.replace(' internal','')
        const interface  = interface_str
          .match(/\(([^\)]*)\)/)[1]
          .split(", ")
          .map(l => l.split(" ")
                   .map(longForm)
          )
          .filter(l => l.length > 1)
        const _interface_types = interface
          .reduce((a, [v, k]) => {
            a["ABI_" + k] = longForm(v);
            return a;
          }, {})
        let fname = interface_str.slice(0, interface_str.indexOf("("))
        a.sig =  fname + "(" + (Object.values(_interface_types)).toString() + ")"
        a.callData = `#abiCallData("${fname}", ${make_args(interface.map(([type, name]) => ({type, name})))}) ++ CD`
        a.signature = fname + "(" + interface
          .map(([t, v]) => t).join(",") + ")"
        a.types = {
          ...a.types,
          ..._interface_types
        }
        a.if = a.internal ? a.if :
          (a.if || []).concat(
                `#sizeWordStack(CD) <=Int 1250000000` //10^4 times the current block gas limit of 8M
          )
        a.interface = interface;
        a.fname = fname;
      } else if ((/^types/.test(head))||(/^for all/.test(head))) {
        // TODO - address will have a contract assigned
        //      - get it and assign ACCT_ID to it
        let _types = tail
          .map(l => l.trim())
          .filter(l => l != "")
          .map(l => l.split(":").map(i => i.trim()))
          .map(([varname, type]) => {
            if(/^address/.test(type)) {
              let contract_name = type.split(" ")[1];
              a.varname2alias = {
                ...a.varname2alias,
                [varname]: contract_name
              }
              a.if = [
                ...(a.if || []),
                `#notPrecompileAddress(${varname})`
              ]
              return [varname, "address"];
            } else {
              return [varname, type];
            }
          })
          .reduce((a, [k, v]) => {
            a[k] = v;
            return a;
          }, {})
        a.types = {
          ...a.types,
          ..._types
        }
      } else if(/^storage/.test(head)) {
        let name = head.split(" ")[1] || "ACCT_ID";
        let _storage = tail
          .map(l => l.trim())
          .filter(l => l != "")
          .map(l => l.split("|->").map(i => i.trim()))
          .map(([k, v]) => {
            if(v.indexOf("=>") > -1) {
              v = v.split("=>").map(i => i.trim())
            } else {
              v = [v, v]
            }
            return [k, v];
          })
          .reduce((a, [k, v]) => {
            a[k] = v;
            return a;
          }, {})
          if( name !== "ACCT_ID" ) {
              a.types[name + "_balance"] = "uint256";
              a.if = [...(a.if || []), `ACCT_ID =/=Int ${name}`]
          }
        a.storage = {
          ...a.storage,
          [name]: _storage
        }
      } else if(/^iff/.test(head)) {
        let _iff = tail
          .map(l => mapInterface(a.interface, toK(l).trim()))
          .filter(l => l != "")
        if(head.indexOf("in range") > -1) {
          let range = bound[head.split(" in range ")[1]]
          _iff = _iff
            .map(l => range(l))
        } else {
          a.iff_orig = e.slice(1)
            .filter(l => l !== "")
            .map(l => l.trim())
        }
        a.iff = (a.iff || []).concat( _iff ).map(s => '('+s+')')
      } else if(/^if/.test(head)) {
        if(tail.join(" ").indexOf("VGas") > -1) warn(`VGas found in the conditions - gas should be handled by klab, try to remove VGas from your constraints`)
        let _if = tail
          .map(l => mapInterface(a.interface, toK(l).trim()))
          .filter(l => l != "")
        a.if = (a.if || []).concat(_if).map(s => '('+s+')')
        a.if_orig = tail
          .filter(l => l != "")
          .map(str => str.trim())
      } else if(/^returns/.test(head)) {
        let returns = head.split(" ").slice(1).join(" ")
          .split(":")
          .map(e => e.trim())
        a.returns = returns
      } else if(/^calls/.test(head)) {
          a.calls = tail
              .map(l => l.trim())
              .filter(l => l != "")
      } else if(/^gas/.test(head)) {
          a.gas = tail
              .map(l => l.trim())
              .filter(l => l != "")
              .map(e => e.split('=>')
                   .map(toK)
                  )[0]
      } else if(/^stack/.test(head)) {
          a.stack = tail
              .map(l => l.trim())
              .filter(l => l != "")
              .map(e => e.split('=>')
                   .map(ws => ws.split(':')
                        .map(l => mapInterface(a.interface, toK(l)))
                        .join(':'))
                  )[0]
      } else if(/^pc/.test(head)) {
          a.pc = tail
              .map(l => l.trim())
              .filter(l => l != "")
              .map(e => e.split('=>'))[0]
      } else if(/^such that/.test(head)) {
         a.ensures = tail
          .map(l => mapInterface(a.interface, toK(l).trim()))
          .filter(l => l != "")
      } else if(!(/^\s*\/\//.test(head))) {
        warn(`WARN: "${head}" block is unknown`)
      }

      return a;
    }, {})
  return act;
}

// * build tests cases
// * enrich with gas conditions
// act2name
const __a2n = act => act.subject + "_" + act.name;
const __hasGas = name => testPath(path.join(KLAB_OUT, "gas", name + ".kast"));
const __getGas = name => read(path.join(KLAB_OUT, "gas", name + ".kast"));
const __getGasRaw = name => JSON.parse(read(path.join(KLAB_OUT, "gas", name + ".raw.kast.json")));

const proofCollection = config => act_collection => act_collection
  // split into pass and fail cases
  .map(act => [{
    act: {
      ...act,
    },
    name: __a2n(act) + (act.lemma ? "_pass" : "_pass_rough"),
    pass: true,
    oog: false,
    rough: !act.lemma,
    gas: act.gas || 3000000,
    gas_raw: act.lemma && act.gas ? `VGas -Int (${act.gas})` : undefined,
    hasGas: act.lemma && !!act.gas
  }].concat("iff" in act ? [{
    act: {...act},
    pass: false,
    oog: false,
    name: __a2n(act) + "_fail"
  }] : [] ))
  .reduce((a, cs) => a.concat(cs), [])
  .map(rule => ({...rule, ...buildAct(config)(rule)}))
  .reduce((a, rule) => ({...a, [rule.name]: rule}), {});

const buildAct = config => ({act, oog, pass, name, hasGas, gas, gas_raw}) => {
  if(config.DEBUG) console.log("build " + name)
  collisionCheck(act)

  // HELPERS
  // TODO - sideeffect free
  const getTerm = ({
    output,
    statusCode,
    accounts,
    gasRewrite,
    pc
  }) => ({
      "k": "#execute ~> CONTINUATION => " + ((act.internal && pass && !(oog)) ? "#execute ~> CONTINUATION" : "#halt ~> CONTINUATION"),
    "ethereum.evm.callState.programBytes": `#parseByteStack("0x${config.contracts[act.subject].bin_runtime}")`,
    "ethereum.evm.callState.program":  `#asMapOpCodes(#dasmOpCodes(#parseByteStack("0x${config.contracts[act.subject].bin_runtime}"), PETERSBURG))`,
    "ethereum.evm.callState.callData": (act.callData || '_') + ` => _`,
    "ethereum.evm.callState.wordStack": wordstack,
    "ethereum.evm.callState.localMem": act.internal ? "_" : ".Map => _",
    "ethereum.evm.callState.pc": pc,
    "ethereum.evm.callState.gas": gasRewrite,
    "ethereum.evm.callState.memoryUsed": act.internal ? "VMemoryUsed" : "0 => _",
    "ethereum.evm.callState.callDepth": "VCallDepth => _",
    "ethereum.evm.output":             output,
    "ethereum.evm.statusCode":         "_ => " + statusCode,
    "ethereum.network.activeAccounts": activeAccounts,
    "ethereum.network.accounts":       accounts,
  })
    const buildAccount = varname => {
        let storage_pass = Object.keys(act.storage[varname])
            .map(key => `[${storagekeys(realname(varname), key)} <- (${clean(act.storage[varname][key][0])} => ${clean(act.storage[varname][key][1])})]`)
            .join('        \n');
        let storage_fail = Object.keys(act.storage[varname])
            .map(key => `[${storagekeys(realname(varname), key)} <- (${clean(act.storage[varname][key][0])} => _)]`)
            .join('        \n');
      let storage = '        \n .Map \n' + ((pass && !oog) ? storage_pass : storage_fail) + '\n  _:Map \n';

    if(!(act.varname2alias[varname])) warn(`Implementation of variable "${varname} : address" not found!`);
    const alias = act.varname2alias[varname];
    if(alias && !(config.implementations[alias])) warn(`Implementation of contract alias ${alias} not found!`);
    const contract_name = alias
      && alias in config.implementations
      && config.implementations[alias].name;
    const balance = act.balance
      && act.balance[varname]
      || varname + "_balance";

    return {account: {
      acctID:  varname,
      balance: balance,
      code:    `#parseByteStack("0x${contract_name && config.contracts[contract_name].bin_runtime || ""}")`,
      storage: storage,
      origStorage: "_",
      nonce:   "_"
    }}
  }

  // STORAGE
  const clean = str => mapInterface(act.interface, toK(str))

  const storagekeys = (name, key) => (/^(\d|\#)/.test(key) ? "" : "#" + name + ".") + clean(key)

  const realname = varname => varname == "ACCT_ID" ? act.subject : varname

  // TODO - simplify this
  if(!act.storage) act.storage = {};
  if(!act.storage.ACCT_ID) act.storage.ACCT_ID = []
  const storage = {
    ...act.storage,
    [act.subject]: act.storage.ACCT_ID
  }
  delete storage.ACCT_ID;

  // GAS
  const gasRewrite = "VGas => " + ((hasGas && !oog) ? gas_raw : "_");
  let gasCond;
  if (pass) {
      gasCond = "VGas " + (oog ? "<Int " : ">=Int ") + gas
  }

  //The multiplication of the list monad
  const flatten = ListOfList => ListOfList.reduce((a, list) => a.concat(list), [])

  // IF
  // make sure used types are within the right range

  const act_if = Object.keys(act.types)
    .filter(name => act.types[name] in bound)
    .map(name => {
      let range = bound[act.types[name]]
      return range(name)
    })
    //assert storage entries have distinct keys from abstract STORE
    .concat(act.if || [])
    .concat(act.internal ? "#rangeUInt(256, VMemoryUsed)" : [])
    .concat(gasCond ? [gasCond]: [])
  const cond_success = (act_if || [])
    .concat(act.iff || [])
    .reduceRight(brackedJoin("\n    andBool ", true), "")
  const cond_fail = (act_if || [])
    .map(c => "  andBool " + c)
    .join("\n")
    + (act.iff ?
       "\n  andBool notBool (\n            "
       + act.iff
       .reduceRight(brackedJoin("\n    andBool "), "")
       + "\n  )"
       : [])
  const cond = pass
    && cond_success
    || cond_fail

  act.ensures = (!pass || oog) ? (act.ensures || []).concat("FAILURE =/=K EVMC_SUCCESS") : act.ensures;
  // PC
  // TODO - do i need this all the time?
  let pc_success = act.pc ? (act.pc.length > 1 ? act.pc.join(" => ") : act.pc[0]) : "0 => _"
  let pc_fail = act.pc ? (act.pc[0] + ' => _') : "0 => _"

  // ACCOUNTS
  const accounts = Object.keys(act.storage)
//    .filter(varname => varname != act.subject)
    .map(buildAccount)
    .concat(["..."])
  const activeAccounts = Object.keys(act.storage)
    .map(name => `SetItem(${name})`)
    .concat('SetItem(1)') //Populate precompile accounts
    .concat('SetItem(2)')
    .concat('SetItem(3)')
    .concat('SetItem(4)')
    .concat('SetItem(5)')
    .concat('SetItem(6)')
    .concat('SetItem(7)')
    .concat('SetItem(8)')
    .join("\n").concat(" _")

  if (act.internal) {
    if(act.interface) {
      let pc_range = srchandler.functionNameToPcRange(act.sig,
                                                        config.contracts[act.subject].srcmapArr,
                                                        config.contracts[act.subject].ast,
                                                        config.contracts[act.subject].bin_runtime,
                                                        config.contracts[act.subject].inst_to_pc
                                                       );
        pc_success = pc_range.join(' => ')
        pc_fail = pc_range[0] + ' => _'
    }
        k_success = "#execute ~> CONTINUATION => #execute ~> CONTINUATION"
        act.callData = '_'
    }

  const wordstack_success = act.stack ? act.stack.join(' => ') : '.WordStack => _'
  const wordstack_fail = act.stack ? act.stack[0] + ' => _' : '.WordStack => _'
  const wordstack = (pass && !oog) && wordstack_success || wordstack_fail;

  const pc = (pass && !oog)
    && pc_success
    || pc_fail

  // OUTPUT
  const buildReturns = rs => rs.length > 0
    ? `#asByteStackInWidthaux(${mapInterface(act.interface, toK(rs[0]))}, 31, 32, ${ buildReturns(rs.slice(1)) })`
    : ".WordStack"
  const output = pass
    && !oog
    && (act.returns
      && ("_ => " + buildReturns(act.returns))
      || ".WordStack")
    || "_ => _";

  // STATUSCODE
  const statusCode = (pass && !oog) ? (act.internal ? "_" : "EVMC_SUCCESS") : "FAILURE:EndStatusCode"

  // IMPORTS
  const imports = (act.calls || [])
    .map(t => t.replace('.','_'))
    .map(t => pass ? [t + "_pass"] : [t + "_pass", t + "_fail"])
    .reduce((a, e) => a.concat(e), [])

  const spec = kjson.renderRule({
    name: act.subject + "_" + act.name,
    requires: cond,
    ensures: act.ensures,
    term: getTerm({
      output,
      statusCode,
      accounts,
      gasRewrite,
      pc
    })
  })

  return {
    spec,
    v2n: act.varname2alias,
    act_name: act.act_name,
    imports
  };
}

const buildActs = (config, act_collection) => {

  var act_proofs = proofCollection(config)(act_collection)

  const getStatus = rule =>
    testPath(path.join(KLAB_OUT, "accept", rule.hash)) && 'accept'
      || testPath(path.join(KLAB_OUT, "timeout", rule.hash)) && 'timeout'
      || testPath(path.join(KLAB_OUT, "reject", rule.hash)) && 'reject'
      || '????'

  const __a2n = act => act.subject + "_" + act.name;
  const __hasGas = hash => testPath(path.join(KLAB_OUT, "gas", hash + ".kast"));
  const __getGas = hash => read(path.join(KLAB_OUT, "gas", hash + ".kast"));
  const __getGasRaw = hash => JSON.parse(read(path.join(KLAB_OUT, "gas", hash + ".raw.kast.json")));

  const forkPass = (obj, rule) => {
    let fork_obj = {
      ...obj,
      act: rule.act,
      pass: true,
      rough: false,
      hasGas: true,
      gas: __getGas(rule.hash),
      gas_raw: kast.format(__getGasRaw(rule.hash), true, true)
    };

    return {
      ...fork_obj,
      ...buildAct(config)(fork_obj)
    };
  }

  var change = true;
  while (change) {
    change = false;

    Object.keys(act_proofs)
      .forEach(name => {
        let rule = act_proofs[name];
        let toClose = !rule.closed && rule.imports
          .map(rule_name =>
            // dependency is available
            act_proofs[rule_name]
            // dependency is closed/ has only proven dependencies
            && act_proofs[rule_name].closed
            // dependency is proven
            && act_proofs[rule_name].status == "accept"
          )
          .reduce((a, e) => a && e, true)

        if (toClose) {
          change = true;
          rule.closed = true;
          // import all dependency into context
          rule.ctx = rule.imports
            .map(rule_name => act_proofs[rule_name]);
          const _rules = [rule.spec].concat(rule.ctx.map(r => r.spec  + "\n[trusted]\n"))
          const module = kjson.renderModule(_rules, rule.name)
          const hash   = config.get_proof_hash(module);
          rule.module  = module;
          rule.hash    = hash;
          rule.status  = getStatus(rule);

          if(rule.status === 'accept' && rule.rough && __hasGas(rule.hash)) {
            let pass_name = name.replace('_rough', '');
            act_proofs[pass_name] = forkPass({
              oog: false,
              name: pass_name
            }, rule)
            if(config.OOG) {
              let oog_name = name.replace('_rough', '_oog');
              act_proofs[oog_name] = forkPass({
                oog: true,
                name: oog_name
              }, rule);
            }
          }
        }
      })
    // let ap = Object.keys(act_proofs)
    //   .reduce((a, n) => ({...a, [n]: {
    //     imports: act_proofs[n].imports,
    //     closed: act_proofs[n].closed,
    //     hash: act_proofs[n].hash,
    //     hasGas: act_proofs[n].hasGas,
    //     gas: act_proofs[n].gas,
    //     status: act_proofs[n].status
    //
    //   }}), {})
    // if(change) console.log(JSON.stringify(ap, false, 2));
  }
  // Object.keys(act_proofs)
  //   .filter(name => !act_proofs[name].status)
  //   .forEach(name => {
  //     console.log(Object.keys(act_proofs[name]))
  //   })

  return act_proofs;

}

module.exports = {
  proofCollection,
  makePrelude,
  makeRules,
  getActs,
  parseAct,
  buildAct,
  buildActs
}
