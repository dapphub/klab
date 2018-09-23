// TODO migrate prelude rules export from klab build into here and make sure make driver does it the same way.
// make sure the new structure works with server/client
const fs            = require("fs");
const path          = require("path");
const marked        = require("marked");
const kjson         = require("../lib/kjson.js");
const {
  testPath,
  read,
  revert,
  warn
}                   = require("./util.js")

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

  fs.writeFileSync("out/prelude.smt2", smt_prelude)
}

const makeRules = config => {

  const rules_template = rules => `requires "edsl.k"
requires "evm.k"

module RULES
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

  fs.writeFileSync("out/rules.k", rules_k);
}

const makeRunScript = () => {
  fs.copyFileSync(path.join(__dirname, "../resources/run.sh"), `out/run.sh`);
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
const parseAct = act_str => {
  let _act = act_str
    .replace(/\/\/[^\n]*\n/g, "")
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
      let tail = e.slice(1)
      if(/^behaviour\s/.test(head)) {
        a.name    = head.split(" ")[1]
        a.subject = head.split(" ")[3]
        a.varname2alias = {
          "ACCT_ID": a.subject
        }
        a.act_name = a.subject + "_" + a.name + ".act";
        console.log("Parsing act " + a.subject + "." + a.name)
        fs.writeFileSync(path.join("out", "acts", a.act_name), act_str)
      } else if(/^interface/.test(head)) {
        const interface_str = head
          .split(" ")
          .slice(1)
          .join(" ");
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
                `ACCT_ID =/= ${varname}`
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
          .map(l => mapInterface(a.interface, l.trim()))
          .filter(l => l != "")
          .map(l => l.split("|->").map(i => toK(i).trim()))
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
        }
        a.iff = (a.iff || []).concat( _iff )
      } else if(/^if/.test(head)) {
        let _if = tail
          .map(l => mapInterface(a.interface, toK(l).trim()))
          .filter(l => l != "")
        a.if = (a.if || []).concat(_if)
      } else if(/^returns/.test(head)) {
        let returns = head.split(" ").slice(1).join(" ")
          .split(":")
          .map(e => e.trim())
        let buildReturns = rs => rs.length > 0
          ? `#asByteStackInWidthaux(${mapInterface(a.interface, toK(rs[0]))}, 31, 32, ${ buildReturns(rs.slice(1)) })`
          : ".WordStack"
        a.returns = buildReturns(returns)
      } else {
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

  const storage_success = Object.keys(act.storage.ACCT_ID)
    .map(key => key + " |-> (" + act.storage.ACCT_ID[key].join(" => ") + ")" )
    .concat(["_:Map"])
    .join("\n  " + " ".repeat(14))
    + "\n"
  const cond_success = (act_if || []).concat(act.iff || [])
    .map(c => "  andBool " + c)
    .join("\n")

  const storage_fail = Object.keys(act.storage.ACCT_ID)
    .map(key => key + " |-> (" + act.storage.ACCT_ID[key][0] + (act.storage.ACCT_ID[key].length > 1 ? " => _ " : "") + ")" )
    .concat(["_:Map"])
    .join("\n  " + " ".repeat(14))
    + "\n"
  const cond_fail = (act_if || [])
    .map(c => "  andBool " + c)
    .join("\n")
    + "\n  andBool notBool (\n            "
    + (act.iff || [])
    .join("\n    andBool ") + "\n  )"

  const accounts = Object.keys(act.storage)
    .filter(varname => varname != "ACCT_ID")
    .map(varname => {
      let fstore = Object.keys(act.storage[varname])
        .map(key => key + " |-> (" + act.storage[varname][key].join(" => ") + ")")
        .concat(["_:Map"])
        .join("\n" + " ".repeat(12))

      if(!(act.varname2alias[varname])) warn(`Implementation of variable "${varname} : address" not found!`);
      let alias = act.varname2alias[varname];
      if(alias && !(config.implementations[alias])) warn(`Implementation of contract alias ${alias} not found!`);
      let contract_name = alias && alias in config.implementations && config.implementations[alias].name;

      return {account: {
        acctID:  varname,
        balance: varname + "_balance",
        code:    `#parseByteStack("0x${contract_name && config.contracts[contract_name].bin_runtime || ""}")`,
        storage: fstore,
        nonce:   "_"
      }}
    }).concat([
      {account: {
        acctID:  "ACCT_ID",
        balance: "BAL",
        code:    `#parseByteStack("0x${config.contracts[act.subject].bin_runtime}")`,
        storage: storage_success || "_",
        nonce:   "_"
      }}
    ])
    .concat(["..."])


  const accounts_fail = Object.keys(act.storage)
    .filter(varname => varname != "ACCT_ID")
    .map(varname => {
      let fstore = Object.keys(act.storage[varname])
        .map(key => key + " |-> (" + act.storage[varname][key][0] + (act.storage[varname][key].length > 1 ? " => _ " : "") + ")" )
        .concat(["_:Map"])
        .join("\n"+" ".repeat(12))

      let alias = act.varname2alias[varname];
      let contract_name = alias && alias in config.implementations && config.implementations[alias].name;

      return {account: {
        acctID:  varname,
        balance: varname + "_balance",
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
    .join("\n")
  // TODO - test if only foreign storage is present, is ACCT_ID in here?

  const filename = act.subject + "_" + act.name

  const getTerm = ({
    output,
    statusCode,
    accounts
  }) => ({
    "k": "#execute => #halt",
    "ethereum.evm.callState.programBytes": `#parseByteStack("0x${config.contracts[act.subject].bin_runtime}")`,
    "ethereum.evm.callState.program":  `#asMapOpCodes(#dasmOpCodes(#parseByteStack("0x${config.contracts[act.subject].bin_runtime}"), BYZANTIUM))`,
    "ethereum.evm.callState.callData": act.callData,
    "ethereum.evm.callState.callValue": "0",
    "ethereum.evm.callState.wordStack": ".WordStack => _",
    "ethereum.evm.callState.pc": "0 => _",
    "ethereum.evm.callState.gas": "VGas => _",
    "ethereum.evm.callState.callDepth": "CALL_DEPTH => _",
    "ethereum.evm.output":             output,
    "ethereum.evm.statusCode":         "_ => " + statusCode,
    "ethereum.network.activeAccounts": activeAccounts,
    "ethereum.network.accounts":       accounts,
  })

  const cases = [{
    name: filename + "_succ",
    spec: kjson.render({
      "name": act.name,
      "filename": filename + "_succ",
      "code": "",
      "term": getTerm({
        output:  act.returns && ("_ => " + act.returns) || ".WordStack",
        statusCode: "EVMC_SUCCESS",
        accounts: accounts
      }),
      requires: cond_success,
    }),
    v2n: act.varname2alias,
    act: act.act_name
  }];

  if((act.iff || []).length > 0) {
    cases.push({
      name: filename + "_fail",
      spec: kjson.render({
        "name": act.name,
        "filename": filename + "_fail",
        "term": getTerm({
          output: "_ => _",
          statusCode: "EVMC_REVERT",
          accounts: accounts_fail
        }),
        "requires": cond_fail
      }),
      v2n: act.varname2alias,
      act: act.act_name
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
  parseAct,
  buildAct
}
