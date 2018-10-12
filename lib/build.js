// TODO migrate prelude rules export from klab build into here and make sure make driver does it the same way.
// make sure the new structure works with server/client
const fs            = require("fs");
const path          = require("path");
const marked        = require("marked");
const kjson         = require("../lib/kjson.js");
const srchandler    = require("./srchandler.js");
const {
  testPath,
  read,
  revert,
  warn
}                   = require("./util.js")
const KLAB_OUT      = process.env.KLAB_OUT || "out";

const STORAGE_DELIMITER = "\n" + " ".repeat(12);
const IFF_DELIMITER      = "";
const brackedJoin = (delimiter, force) => (a, str, i) => (i > 0 || force ? delimiter : "") + `(${str + a})`

const getActs = str => {
  const raw_config = marked.lexer(str)

  const acts_str = raw_config
    .filter(e => e.type === "code")
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
  if(smt_prelude_old != smt_prelude) {
    console.log("writing to " + prelude_out_path);
    fs.writeFileSync(prelude_out_path, smt_prelude)
  }
}

const makeRules = config => {

  const rules_template = rules => `requires "domains.k"
requires "edsl.k"
requires "evm.k"

module RULES
    imports BYTES-SYMBOLIC
    imports EVM
    imports EDSL

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
  if(rules_k_old != rules_k) {
    console.log("write to " + rules_path);
    fs.writeFileSync(rules_path, rules_k);
  }

}

const makeRunScript = () => {
  fs.copyFileSync(path.join(__dirname, "../resources/run.sh"), path.join(KLAB_OUT, `run.sh`));
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

const bound = {
  "uint48" : name => `#rangeUInt(48, ${ name })`,
  "uint256": name => `#rangeUInt(256, ${ name })`,
  "int256" : name => `#rangeSInt(256, ${ name })`,
  "bytes32": name => `#rangeBytes(32, ${ name })`,
  "address": name => `#rangeAddress(${ name })`
}

const make_abi_dsl = ({type, name}, i) =>
  "#" + type + "(ABI_" + name + ")"
const make_args = inputs => inputs && inputs.length == 0
    && ".TypedArgs"
    || inputs.map(make_abi_dsl).join(", ")
const mapInterface = (is, str) => {
  is.forEach(([t, n]) => {
    str = str.replace(new RegExp("([^\\w\.]|^)" + n + "([^\\w]|$)", "g"), (a, b, c, d) => {
      return b + "ABI_" + n + c;
    })
  })
  return str;
}
const parseAct = (act_str, silent = false) => {
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
        if(!silent) console.log("Parsing act " + a.subject + "." + a.name)
        fs.writeFileSync(path.join(KLAB_OUT, "acts", a.act_name), act_str)
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
        const interface_str = head
          .split(" ")
          .slice(1)
          .join(" ");
        a.internal = interface_str.split(' ').slice(-1) == 'internal' ? true : false;
        interface_str = interface_str.replace(' internal','')
        const interface  = interface_str
          .match(/\(([^\)]*)\)/)[1]
          .split(", ")
          .map(l => l.split(" "))
          .filter(l => l.length > 1)
        const _interface_types = interface
          .reduce((a, [v, k]) => {
            a["ABI_" + k] = v;
            return a;
          }, {})
        const fname = interface_str.slice(0, interface_str.indexOf("("))
        a.callData = `#abiCallData("${fname}", ${make_args(interface.map(([type, name]) => ({type, name})))})`
        a.signature = fname + "(" + interface
          .map(([t, v]) => t).join(",") + ")"
        a.types = {
          ...a.types,
          ..._interface_types
        }
        a.interface = interface;
        a.fname = fname;
      } else if(/^types/.test(head)) {
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
                `#notPrecompileAddress(${varname})`,
                `ACCT_ID =/=Int ${varname}`
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
              v = [v]
            }
            return [k, v];
          })
          .reduce((a, [k, v]) => {
            a[k] = v;
            return a;
          }, {})
        if( name !== "ACCT_ID" ) a.types[name + "_balance"] = "uint256";
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
        a.iff = (a.iff || []).concat( _iff )
      } else if(/^if/.test(head)) {
        let _if = tail
          .map(l => mapInterface(a.interface, toK(l).trim()))
          .filter(l => l != "")
        a.if = (a.if || []).concat(_if)
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
      } else if(!(/^\s*\/\//.test(head))) {
        warn(`WARN: "${head}" block is unknown`)
      }

      return a;
    }, {})
  return act;
}




const buildAct = config => act => {
  console.log("Building k-spec " + act.subject + "." + act.name)

  if(!act.storage) act.storage = {};
  if(!act.storage.ACCT_ID) act.storage.ACCT_ID = []

  const act_if = Object.keys(act.types)
    .filter(name => act.types[name] in bound)
    .map(name => {
      let range = bound[act.types[name]]
      return range(name)
    })
    .concat(act.if || [])
    .concat(act.internal ? "#rangeUInt(256, VMemoryUsed)" : [])

  const _i = str => mapInterface(act.interface, toK(str))
  const storageEntry = (subject, key, vals, fail) => {
    if(fail) vals = [vals[0]]
      .concat(
        vals.length > 1
        ? ["_"]
        : []
      );
    return (/^(\d|\#)/.test(key) ? "" : `#${subject}.`)
      + _i(key)
      + " |-> ("
      + vals.map(_i).join(" => ")
      + ")";
  }


  const storage_success = Object.keys(act.storage.ACCT_ID)
    .map(key => storageEntry(act.subject, key, act.storage.ACCT_ID[key]))
    .concat(["_:Map"])
    .reverse()
    .reduceRight(brackedJoin(STORAGE_DELIMITER), "")
    + "\n"
  const cond_success = (act_if || []).concat(act.iff || [])
    .reduceRight(brackedJoin("\n    andBool ", true), "")
    // .map(c => "  andBool " + c)
    // .join("\n")

  const storage_fail = Object.keys(act.storage.ACCT_ID)
    .map(key => storageEntry(act.subject, key, act.storage.ACCT_ID[key], true))
    .concat(["_:Map"])
    .reverse()
    .reduceRight(brackedJoin(STORAGE_DELIMITER), "")
        + "\n"

  const gas_success = act.gas ? (act.gas.length > 1 ? act.gas.join(" => ") : act.gas[0]) : "VGas => _"

  const gas_fail = act.gas ? (act.gas[0] + ' => _') : "VGas => _"

  const pc_success = act.pc ? (act.pc.length > 1 ? act.pc.join(" => ") : act.pc[0]) : "0 => _"
      
  const pc_fail = act.pc ? (act.pc[0] + ' => _') : "0 => _"


  const cond_fail = (act_if || [])
    .map(c => "  andBool " + c)
    .join("\n")
    + "\n  andBool notBool (\n            "
    + (act.iff || [])
    // .map((str, i) => (i == 0 ? "" : "") + str)
    .reverse()
    .reduceRight(brackedJoin("\n    andBool "), "")
    + "\n  )"

  const accounts = Object.keys(act.storage)
    .filter(varname => varname != "ACCT_ID")
    .map(varname => {
      let fstore = Object.keys(act.storage[varname])
        .map(key => storageEntry(varname, key, act.storage[varname][key]))
        .concat(["_:Map"])
        .reverse()
        .reduceRight(brackedJoin(STORAGE_DELIMITER), "")

      if(!(act.varname2alias[varname])) warn(`Implementation of variable "${varname} : address" not found!`);
      let alias = act.varname2alias[varname];
      if(alias && !(config.implementations[alias])) warn(`Implementation of contract alias ${alias} not found!`);
      let contract_name = alias && alias in config.implementations && config.implementations[alias].name;
      let balance = act.balance && act.balance[varname] || varname + "_balance";

      return {account: {
        acctID:  varname,
        balance: balance,
        code:    `#parseByteStack("0x${contract_name && config.contracts[contract_name].bin_runtime || ""}")`,
        storage: fstore,
        nonce:   "_"
      }}
    }).concat([
      {account: {
        acctID:  "ACCT_ID",
        balance: act.balance && act.balance.ACCT_ID || "BAL",
        code:    `#parseByteStack("0x${config.contracts[act.subject].bin_runtime}")`,
        storage: storage_success || "_",
        nonce:   "_"
      }}
    ])
    .concat(["..."])

    let k_success = "#execute ~> CONTINUATION => #halt ~> CONTINUATION"
    if (act.internal) {
        let pc_range = srchandler.functionNameToPcRange(act.name,
                                                        config.contracts[act.subject].srcmapArr,
                                                        config.contracts[act.subject].ast,
                                                        config.contracts[act.subject].bin_runtime
                                                       );
        pc_success = pc_range.join(' => ')
        pc_fail = pc_range[0] + ' => _'
        k_success = "#execute ~> CONTINUATION => #execute ~> CONTINUATION"
        act.callData = '_'
    }

  const wordstack_success = act.stack ? act.stack.join(' => ') : '.WordStack => _'
  const wordstack_fail = act.stack ? act.stack[0] + ' => _' : '.WordStack => _'
    
  const accounts_fail = Object.keys(act.storage)
    .filter(varname => varname != "ACCT_ID")
    .map(varname => {
      let fstore = Object.keys(act.storage[varname])
        .map(key => storageEntry(varname, key, act.storage[varname][key], true))
        .concat(["_:Map"])
        .reverse()
        .reduceRight(brackedJoin(STORAGE_DELIMITER), "")

      let alias = act.varname2alias[varname];
      let contract_name = alias && alias in config.implementations && config.implementations[alias].name;

      return {account: {
        acctID:  varname,
        balance: varname + "_balance => _" ,
        code:    `#parseByteStack("0x${contract_name && config.contracts[contract_name].bin_runtime || ""}")`,
        storage: fstore,
        nonce:   "_"
      }}
    }).concat([
      {account: {
        acctID:  "ACCT_ID",
        balance: "BAL",
        code:    `#parseByteStack("0x${config.contracts[act.subject].bin_runtime}")`,
        storage: storage_fail || "_",
        nonce:   "_"
      }}
    ])
    .concat(["..."])

  const activeAccounts = Object.keys(act.storage)
    .map(name => `SetItem(${name})`)
    .join("\n").concat(" _")
  // TODO - test if only foreign storage is present, is ACCT_ID in here?

  const filename = act.subject + "_" + act.name

  const getTerm = ({
    k,
    output,
    statusCode,
    accounts,
    gas,
    pc,
    wordstack
  }) => ({
    "k": k,
    "ethereum.evm.callState.programBytes": `#parseByteStack("0x${config.contracts[act.subject].bin_runtime}")`,
    "ethereum.evm.callState.program":  `#asMapOpCodes(#dasmOpCodes(#parseByteStack("0x${config.contracts[act.subject].bin_runtime}"), BYZANTIUM))`,
    "ethereum.evm.callState.callData": act.callData,
    "ethereum.evm.callState.callValue": "0",
    "ethereum.evm.callState.wordStack": wordstack,
    "ethereum.evm.callState.localMem": act.internal ? "_ => _" : ".Map => _",
    "ethereum.evm.callState.pc": pc,
    "ethereum.evm.callState.gas": gas,
    "ethereum.evm.callState.memoryUsed": act.internal ? "VMemoryUsed" : "0 => _",
    "ethereum.evm.callState.callDepth": "VCallDepth => _",
    "ethereum.evm.output":             output,
    "ethereum.evm.statusCode":         "_ => " + statusCode,
    "ethereum.network.activeAccounts": activeAccounts,
    "ethereum.network.accounts":       accounts,
  })

  const buildReturns = rs => rs.length > 0
    ? `#asByteStackInWidthaux(${mapInterface(act.interface, toK(rs[0]))}, 31, 32, ${ buildReturns(rs.slice(1)) })`
    : ".WordStack"

  const cases = [{
    name: filename + "_succ",
    spec: kjson.renderRule({
      "name": act.subject + "_" + act.name,
      "filename": filename + "_succ",
      "code": "",
      "term": getTerm({
        k: k_success,
        output:  act.returns && ("_ => " + buildReturns(act.returns)) || ".WordStack",
        statusCode: act.internal ? "_" : "EVMC_SUCCESS",
        accounts: accounts,
        gas: gas_success,
        pc: pc_success,
        wordstack: wordstack_success
      }),
      requires: cond_success,
    }),
    v2n: act.varname2alias,
    act: act.act_name,
    imports: (act.calls || []).map(t => t.replace('.','_'))
  }];

  if((act.iff || []).length > 0) {
    cases.push({
      name: filename + "_fail",
      spec: kjson.renderRule({
        "name": act.subject + "_" + act.name,
        "filename": filename + "_fail",
        "term": getTerm({
          k: "#execute ~> CONTINUATION => #halt ~> CONTINUATION",
          output: "_ => _",
          statusCode: "EVMC_REVERT",
          accounts: accounts_fail,
          gas: gas_fail,
          pc: pc_fail,
          wordstack: wordstack_fail
            
        }),
        "requires": cond_fail
      }),
      v2n: act.varname2alias,
      act: act.act_name,
      imports: (act.calls || []).map(t => t.replace('.','_'))
    })
  }

  // cases
  //   .forEach(c => {
  //     out[c.name] = {
  //       v2n: act.varname2alias,
  //       act: act.act_name
  //     }
  //     // if(c.name == "Vat_move_fail") {
  //     //   console.log("\n...\n"+c.spec+"\n..\n");
  //     // }
  //   })
  return cases;
}

module.exports = {
  makePrelude,
  makeRules,
  makeRunScript,
  getActs,
  parseAct,
  buildAct
}
