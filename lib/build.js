// TODO migrate prelude rules export from klab build into here and make sure make driver does it the same way.
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
  toK,
  sha3,
  getStatus
}                   = require("./util.js")
const {
  collisionCheck,
  getStorageDef
} = require("./storage.js");
const KLAB_OUT      = process.env.KLAB_OUT || "out";

const STORAGE_DELIMITER = "\n" + " ".repeat(12);
const IFF_DELIMITER      = "";
const brackedJoin = (delimiter, force) => (a, str, i) => (i > 0 || force ? delimiter : "") + `(${str + a})`

const BIG_GAS = 3000000;

const h = n => s => `<${Array.isArray(n) ? n.join(' ') : n}>${Array.isArray(s) && s.join('') || s}</${Array.isArray(n) ? n[0] : n}>`
const newCleanup = (act, text) => {

  let storages_blocks = Object
    .keys(act.storage || {})
    .map(subjectName => {
      // TODO - export this one level higher - all storages
      let isCreate = act.creates[subjectName];
      let max_key_length = Object
        .keys(act.storage[subjectName])
        .map(key => key.length)
        .reduce((a, l) => Math.max(a, l), 0);
      let max_lhs_length = Object
        .keys(act.storage[subjectName])
        .map(key => act.storage[subjectName][key][0].length)
        .reduce((a, l) => Math.max(a, l), 0);
      let storage = Object
        .keys(act.storage[subjectName])
        .map(key => {
          let rewrites = act.storage[subjectName][key];
          const key_ = /^(\d|\#)/.test(key) ? key : `#${subjectName == "ACCT_ID" ? act.subject : subjectName}.${key}`
          const def = getStorageDef(key_);
          if(!def && false) {
            warn(`Storage key ${key_} in ${act.subject}.${act.name} not declared!`)
          }
          rewrites[0] = rewrites[0] + " ".repeat(max_lhs_length - rewrites[0].length)
          let storage_str = "  "
            + key
            + " ".repeat(max_key_length - key.length)
            + " |-> "
            + (isCreate ? rewrites[0] : rewrites.join(" => "))
          let comment_str = def && def.doc(key_) && ("  // " + def.doc(key_) + "\n") || ""
          return comment_str + storage_str;
        })
        .join("\n")

      // Return the storage block header + body
      // - append the contract name to the header when referencing external contracts
      // - preserve the creates header when present
      let prefix = isCreate ? "creates storage" : "storage"
      let header = prefix + (subjectName !== "ACCT_ID" ? " " + subjectName : "");
      return header + "\n\n" + storage + "\n";
    })
    .join("\n\n")

  const code_ = text
    .split(/\n/)
    .reduce(({isStorage, display}, line) => {
      const isStorageBlock = (/^storage$/).test(line);
      const isOtherStorageBlock = (/^(storage|creates storage)\s+\w/).test(line);
      const isBlock = (/^\w/).test(line);
      const isStorage_ = isStorageBlock || isOtherStorageBlock || (isStorage && !isBlock);

      return {
        isStorage: isStorage_,
        display: display
          .concat(
            isStorage_ && !isStorageBlock
            ? []
            : isStorageBlock && storages_blocks || line
          )
      };
    }, {
      isStorage: false,
      display: []
    })
    .display
    .join("\n")
    .replace(/\/\/ doc:/g, "//")
    .replace(/\/\/ act:/g, "//")
    .replace(/\/\/(.*)\n/gm, (_1, _2, l) => `<span class="comment">//${_2}</span>\n`)
    .replace(/\s*\`[^\?\`]*\?([^\:\`]*)\:[^\`]*\`\s*/g, (_1, s) => s.trim() == "" ? " " : ` ${s.trim()} `)
    .replace(/\`([^\`]*)\`/g, (_, s) => `<span class="var">${s}</span>`)
    .replace(/<</, "&lt;&lt;")


  const id = act.subject + "_" + act.sig;

  const header = `<div class="codeheader">
      ${h(['a', `class="anchor" href="#${id}"`])('⚓')}
      ${h(['a', `class="jumptop" href="#top"`])('top')}
    </div>`;

  return `<div id="${id}">${header}<pre><code>${code_}</code></pre></div>`;
}

const getActs = str => {
  const raw_config = marked.lexer(str)

  // clean literate programming md file
  const acts_str = raw_config
    .filter(e => e.type === "code" && e.lang == "act")
    .map(e => e.text)
    .join("\n\n")

  // split for acts
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

const makeRules = (config, dirty = false) => {

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

  const dirty_rule_strs = dirty
    && config.src.dirty_rules
    && config.src.dirty_rules
      .map(read)
      .map((rule_str, i) => "```\n// " + config.src.dirty_rules[i] + "\n\n```\n" + rule_str)
      .join("\n")
    || "";

  const rules = marked.lexer(dirty_rule_strs + rule_strs)
    .filter(e => e.type === "code")
    .map(e => e.text)
    .join("\n")

  const rules_k = rules_template([
    fs.readFileSync(path.join(__dirname, "../resources/rules.k.tmpl")).toString(),
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

const makeInterabiExhaustiveness = (alias, cname, contract, hasher) => {
  const if_not_entries = contract.abi
    .filter(fabi => fabi.type == "function")
    .map(fabi => sha3(fabi.name + `(${fabi.inputs.map(i => i.type).join(",")})`).slice(0, 8))
    .map(fabis => parseInt(fabis, 16).toString())
    .map(abiexpr => `andBool ${abiexpr} =/=Int #asWord(#take(4, #take(32, VCallData)))`)
    .join("\n")
    .concat(`\nandBool VGas >=Int 40000000`)

  const spec = kjson.renderRule({
    name: alias + "__exhaustiveness",
    fullname: alias + "__exhaustiveness",
    requires: if_not_entries,
    ensures: "",
    term: {
      k                                     : "#execute ~> CONTINUATION => #halt ~> CONTINUATION",
      "ethereum.evm.callState.program"      : `${alias}_bin_runtime`,
      "ethereum.evm.callState.jumpDests"    : `#computeValidJumpDests(${alias}_bin_runtime)`,
      "ethereum.evm.callState.wordStack"    : ".WordStack => ?_",
      "ethereum.evm.callState.localMem"     : ".Map => ?_",
      "ethereum.evm.callState.pc"           : "0 => ?_",
      "ethereum.evm.statusCode"             : "_ => EVMC_REVERT_NETWORK",
      "ethereum.evm.callState.gas"          : "VGas => ?_",
      "ethereum.network.accounts"           : "_"
    }
  })

  const id = hasher({name: cname, spec: spec});

  const status = getStatus(id);

  const module = kjson.renderModule([spec], id)

  return {
    id,
    module,
    status
  };
}

const parseAct = config => (act_str, silent = false) => {
  let _act = act_str
    .replace(/ \\\n/gm, "") // allow for simple bash style line continuations
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
      if(/^behaviour\s/.test(head) || /^failure\s/.test(head)) {
        a.name    = head.split(" ")[1]
        a.subject = head.split(" ")[3]
        a.varname2alias = {
          "ACCT_ID": a.subject
        }
        a.act_name = a.subject + "_" + a.name + ".act";
        a.only_fail = (/^failure\s/.test(head));
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
                `#sizeByteArray(CD) <=Int 1250000000` //10^4 times the current block gas limit of 8M
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
      } else if(/(^)(storage|creates storage)/.test(head)) {
        let name = head.split("storage ")[1] || "ACCT_ID";
        let creates = /^creates/.test(head);
        let isStatic = tail.reduce((a, l) => a && !(/=>/.test(l)), true)
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
        a.isStatic = a.isStatic && isStatic;
        a.creates = {
          ...(a.creates || {}),
          [name]: creates
        }
        a.storage = {
          ...a.storage,
          [name]: _storage
        }
        if(creates && name !== "ACCT_ID") {
          a.varname2alias = {
            ...a.varname2alias,
            ["#newAddrCreate2(#newAddrCreate2)"]: a.varname2alias[name],
            ["#newAddr(ACCT_ID, Nonce_"+ a.varname2alias['ACCT_ID'] + ")"]: a.varname2alias[name]
          }
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
      } else if(/^returns\s/.test(head)) {
        let returns = head.split(" ").slice(1).join(" ")
          .split(":")
          .map(e => e.trim())
        a.returns = returns
      } else if(/^returnsRaw/.test(head)) {
        a.returnsRaw = head.split(" ").slice(1).join(" ")
      } else if(/^where/.test(head)) {
          a.whereBindings = tail
              .map(l => l.trim())
              .filter(l => l != "")
              .map(l => mapInterface(a.interface, toK(l).trim()))
              .map(l => ({
                /* matches the LHS of the macro if it is:
                 * preceded by:
                 *  - ', ' or ','
                 *  - '('
                 *  - '['
                 *  - the start of a line
                 *  - a tab
                 *  - a space
                 * and followed by:
                 *  - ','
                 *  - ')'
                 *  - ']'
                 *  - the end of a line
                 *  - a tab
                 *  - a space
                 *
                 *  example here: https://regex101.com/r/ZubkN1/4
                 */
                "from": new RegExp(`(, ?|\\(|\\[|^|\\t| )(${l.split(":=")[0].trim()})(,|\\)|$|\\t|\\]| )`, "gm"),
                /*
                 * replaces with the first capture group from the expression
                 * above ('$1'), then the RHS of the macro (surrounded by
                 * brackets), then the third capture group from above ('$3')
                 */
                "to": `$1(${l.split(":=")[1].trim()})$3`
              }))

            // apply every where binding in turn to every other where binding
            for (i = 0; i < a.whereBindings.length; i++) {
              for (j = 0; j < a.whereBindings.length; j++) {
                a.whereBindings[j].to = a.whereBindings[j].to.replace(a.whereBindings[i].from, a.whereBindings[i].to);
              }
            }
      } else if(/^calls/.test(head)) {
          a.calls = tail
              .map(l => l.trim())
              .filter(l => l != "")
          a.if = (a.if || [])
              .concat(a.calls.map(l => l.split(".")[0])
              .filter(l => l != a.subject)
              .filter((l, i, a) => a.indexOf(l) === i)
              .map(l => `(${l} =/=Int 0)`))
      } else if(/^gas/.test(head)) {
          a.gas = tail
              .map(l => l.trim())
              .filter(l => l != "")
              .map(e => e.split('=>')
                   .map(toK)
                  )[0]
      } else if(/^fail_gas/.test(head)) {
          a.fail_gas = tail
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
    }, {
      isStatic: true
    })
  return act;
}

// * build tests cases
// * enrich with gas conditions
// act2name
const __a2n = act => act.subject + "_" + act.name;
const __hasGas = name => testPath(path.join(KLAB_OUT, "gas", name + ".all.json"));
const __getBehaviour = (id, hash) => read(path.join(KLAB_OUT, "behaviour", hash + "." + id));
const __getJsonGas = name => JSON.parse(read(path.join(KLAB_OUT, "gas", name + ".all.json")));


const caseSplitAct = config => act => {
  const cases = [];
  const isOOGAct = (config.oog || []).indexOf(__a2n(act)) > -1
  const canPass = !act.only_fail;
  const canFail = "iff" in act;

  const gas = act.gas ? [`VGas -Int (${act.gas})`] : [BIG_GAS]

  // 1. build pass case
  if(canPass) {
    cases.push({
      act: {...act},
      name: __a2n(act) + (!!act.gas ? "_pass" : "_pass_rough"),
      pass: true,
      rough: !act.lemma,
      oog: false,
      gas,
      hasGas: !!act.gas
    })
  }

  // 2. build fail cases
  // 2.2. build big fail case
  if(canFail) {
    cases.push({
      act: {...act},
      pass: false,
      gas: act.gas || BIG_GAS,
      oog: isOOGAct,
      name: __a2n(act) + "_fail" + ((!isOOGAct) ? '_rough' : '')
    })
  }

  return cases;
}



const proofCollection = config => act_collection => act_collection
  // split into pass and fail cases
  // .map(caseSplitAct(config))
  // .reduce((a, cs) => a.concat(cs), [])
  .map(rule => ({...rule, ...buildAct(config)(rule)}))
  .reduce((a, rule) => ({...a, [rule.name]: rule}), {});


const getTerm = ({
  alias,
  output,
  statusCode,
  accounts,
  gasRewrite,
  pc,
  activeAccounts,
  pass,
  internal,
  oog,
  bin_runtime,
  callData,
  wordstack,
  isStatic
}) => ({
  "k": "#execute ~> CONTINUATION => " + ((internal && pass && !(oog)) ? "#execute ~> CONTINUATION" : "#halt ~> CONTINUATION"),
  "ethereum.evm.callState.program"      : `${alias}_bin_runtime`,
  "ethereum.evm.callState.jumpDests"    : `#computeValidJumpDests(${alias}_bin_runtime)`,
  "ethereum.evm.callState.callData"     : (callData || '_') + ` => ?_`,
  "ethereum.evm.callState.wordStack"    : wordstack,
  "ethereum.evm.callState.localMem"     : internal ? "_" : ".Map => ?_",
  "ethereum.evm.callState.pc"           : pc,
  "ethereum.evm.callState.gas"          : gasRewrite,
  "ethereum.evm.callState.memoryUsed"   : internal ? "VMemoryUsed" : "0 => ?_",
  "ethereum.evm.callState.callDepth"    : "VCallDepth" + (pass ? "" : " => ?_"),
  "ethereum.evm.output"                 : output,
  "ethereum.evm.statusCode"             : "_ => " + statusCode,
  "ethereum.network.activeAccounts"     : activeAccounts,
  "ethereum.network.accounts"           : accounts,
  "ethereum.evm.callState.static"       : isStatic ? '_' : 'false'
})

const buildAct = config => ({act, oog, pass, name, hasGas, gas, lemma}) => {
  if(config.DEBUG) console.log("build " + name)
  // TODO - put this out
  collisionCheck(act)

  var junk = 0;

  const remember_junk = vname => vname.trim() == "_" && ("Junk_" + (junk++)) || vname.trim()
  const buildAccount = varname => {

    const isPass = pass && !oog;
    const isCreate = act.creates !== undefined && act.creates[varname];
    // Set origStorage to V <- K, and restrict K to be in range of a uint256.
    const origStorage_str = (key) => {
      let lhs = `${storagekeys(realname(varname), key)} <- (Junk_${junk++})`
      return `[${isCreate ? "0" : lhs}]`
    }
    const storage_str = (key) => {
      let lhs = `${storagekeys(realname(varname), key)} <- (${remember_junk(clean(act.storage[varname][key][0]))})`
      let rhs = `=> ${isPass ? clean(act.storage[varname][key][1]) : "?_"}`
      return `[${lhs} ${isCreate ? "" : rhs}]`
    }

    const storage_strs = Object.keys(act.storage[varname])
      .map(key => storage_str(key))
      .join('        \n');

    const origStorage_strs = Object.keys(act.storage[varname])
      .map(key => origStorage_str(key))
      .join('        \n');

    var storage, origStorage;
    if(isCreate && pass) {
      storage = `        \n .Map => (.Map\n${ storage_strs }\n)\n`;
      origStorage = `        \n .Map\n`;
    } else if (isCreate) {
      storage = `        \n .Map => ?_\n`;
      origStorage = `        \n .Map\n`;
    } else {
      storage = `        \n .Map \n${ storage_strs }\n  _:Map\n`;
      origStorage = `        \n .Map\n${ origStorage_strs }\n  _:Map\n`;
    }

    // TODO - which proof is this coming from?
    if(!(act.varname2alias[varname])) warn(`Implementation of variable "${varname} : address" not found in proof ${name}`);
    const alias = act.varname2alias[varname];
    if(alias && !(config.implementations[alias])) warn(`Implementation of contract alias ${alias} not found!`);
    const contract_name = alias
      && alias in config.implementations
      && config.implementations[alias].name;
    const balance = act.balance
      && act.balance[varname]
      || varname + "_balance";
    const code = isCreate && isPass && `.WordStack => ${alias}_bin_runtime` || isCreate && ".WordStack => ?_" || alias + "_bin_runtime"
    const nonce = (isCreate && isPass)
      && "0 => 1"
      || "Nonce_" + alias + " => ?_"

    return {account: {
      acctID:  varname,
      balance: balance,
      code:    code,
      storage: storage,
      origStorage: origStorage,
      nonce:   nonce
    }}
  }

  // STORAGE
  const clean = str => mapInterface(act.interface, toK(str))

  const storagekeys = (name, key) => clean((/^(\d|\#)/.test(key) ? "" : "#" + name + ".") + key)

  const realname = varname => varname == "ACCT_ID" ? act.subject : act.varname2alias[varname]

  // TODO - simplify this
  if(!act.storage) act.storage = {};
  if(!act.storage.ACCT_ID) act.storage.ACCT_ID = []
  const storage = {
    ...act.storage,
    [act.subject]: act.storage.ACCT_ID
  }
  delete storage.ACCT_ID;

  // GAS
  const gasRewrite = "VGas => " + ((hasGas && !oog) ? gas[0] : "?_");
  let gasCond;
  if (hasGas && pass) {
      // TODO : oog?!
      gasCond = (Array.isArray(gas) && gas || [gas])
        // Account for EIP-2200's definiition of 2300 gasleft as the lower bound for SSTOREs.
        .map(g => "2300 " + (oog ? ">=Int " : "<Int ") + g)
  } else if (hasGas && !pass && !oog) {
      gasCond = (Array.isArray(gas) && gas || [gas])
        .map(g => "0 <Int " + g)
  } else if(!pass && act.fail_gas) {
    gasCond = ["VGas >=Int " + act.fail_gas];
  } else if(!hasGas) {
    gasCond = [`VGas >=Int ${BIG_GAS}`]
  }

  //The multiplication of the list monad
  const flatten = ListOfList => ListOfList.reduce((a, list) => a.concat(list), [])


  const precompile1 = {
    account: {
      acctID:  1,
      balance: 'ECREC_BAL',
      code:    '.WordStack',
      storage: '_:Map',
      origStorage: "_",
      nonce:   "_"
    }
  }
  const precompile2 = {
    account: {
      acctID:  2,
      balance: 'SHA256_BAL',
      code:    '.WordStack',
      storage: '_:Map',
      origStorage: "_",
      nonce:   "_"
    }
  }
  const precompile3 = {
    account: {
      acctID:  3,
      balance: 'RIP160_BAL',
      code:    '.WordStack',
      storage: '_:Map',
      origStorage: "_",
      nonce:   "_"
    }
  }
  const precompile4 = {
    account: {
      acctID:  4,
      balance: 'ID_BAL',
      code:    '.WordStack',
      storage: '_:Map',
      origStorage: "_",
      nonce:   "_"
    }
  }
  const precompile5 = {
    account: {
      acctID:  5,
      balance: 'MODEXP_BAL',
      code:    '.WordStack',
      storage: '_:Map',
      origStorage: "_",
      nonce:   "_"
    }
  }
  const precompile6 = {
    account: {
      acctID:  6,
      balance: 'ECADD_BAL',
      code:    '.WordStack',
      storage: '_:Map',
      origStorage: "_",
      nonce:   "_"
    }
  }
  const precompile7 = {
    account: {
      acctID:  7,
      balance: 'ECMUL_BAL',
      code:    '.WordStack',
      storage: '_:Map',
      origStorage: "_",
      nonce:   "_"
    }
  }
  const precompile8 = {
    account: {
      acctID:  8,
      balance: 'ECPAIRING_BAL',
      code:    '.WordStack',
      storage: '_:Map',
      origStorage: "_",
      nonce:   "_"
    }
  }
  const precompile9 = {
    account: {
      acctID:  9,
      balance: 'BLAKE2_BAL',
      code:    '.WordStack',
      storage: '_:Map',
      origStorage: "_",
      nonce:   "_"
    }
  }

  const accounts = Object.keys(act.storage)
    .map(buildAccount)
    .concat(precompile1)
    .concat(precompile2)
    .concat(precompile3)
    .concat(precompile4)
    .concat(precompile5)
    .concat(precompile6)
    .concat(precompile7)
    .concat(precompile8)
    .concat(precompile9)
    .concat(["..."])

  // IF
  // make sure used types are within the right range

  const act_if = Object.keys(act.types)
    .filter(name => act.types[name] in bound)
    .map(name => {
      let range = bound[act.types[name]]
      return range(name)
    })
    .concat(act.if || [])
    .concat(act.internal ? "#rangeUInt(256, VMemoryUsed)" : [])
    .concat(gasCond ? gasCond : [])
    .concat((new Array(junk)).fill(1).map((a, i) => `#rangeUInt(256, Junk_${i})`))
  const pos_cond = (act_if || [])
    .concat(pass && act.iff || [])
    .reduceRight(brackedJoin("\n  andBool ", true), "")
  const known_vars = [
    'VCallValue',
    'ACCT_ID',
    'CALLER_ID',
    'VGas',
    'VCallDepth',
    'TIME'
  ];
  const neg_cond = (pass && []) || act.iff
  const neg_cond_str = neg_cond
    .map(c => Object.keys(act.types).concat(known_vars).reduce((cond, v) => cond.replace(new RegExp(`\\b${v}(\\b[^:])`, "g"), (a, char) => v + ":Int" + char) , c))
    .reduceRight(brackedJoin("\n    andBool "), "")

  const cond = pos_cond
    + (neg_cond.length > 0 && `\n  andBool notBool (\n    ${ neg_cond_str }\n  )` || "")


  act.ensures = (!pass || oog) ? (act.ensures || []).concat("?FAILURE =/=K EVMC_SUCCESS") : act.ensures;
  // PC
  // TODO - do i need this all the time?
  let pc_success = act.pc ? (act.pc.length > 1 ? act.pc.join(" => ") : act.pc[0]) : "0 => ?_"
  let pc_fail = act.pc ? (act.pc[0] + ' => ?_') : "0 => ?_"

  // ACCOUNTS
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
    if((!act.pc) && act.interface) {
      let pc_range = srchandler.functionNameToPcRange(act.sig,
                                                        config.contracts[act.subject].bin_runtime_srcmapArr,
                                                        config.contracts[act.subject].ast,
                                                        config.contracts[act.subject].bin_runtime,
                                                        config.contracts[act.subject].bin_runtime_inst_to_pc
                                                       );
      if (pc_range[0] === undefined || pc_range[1] === undefined) warn(`Could not extract pc value for ${act.subject}.${act.sig}`)
        pc_success = pc_range.join(' => ')
        pc_fail = pc_range[0] + ' => ?_'
    }
        k_success = "#execute ~> CONTINUATION => #execute ~> CONTINUATION"
        act.callData = '_'
    }

  const wordstack_success = act.stack ? act.stack.join(' => ') : '.WordStack => ?_'
  const wordstack_fail = act.stack ? act.stack[0] + ' => ?_' : '.WordStack => ?_'
  const wordstack = (pass && !oog) && wordstack_success || wordstack_fail;

  const pc = (pass && !oog)
    && pc_success
    || pc_fail

  // OUTPUT
  const buildReturns = ( rs ) => rs.length > 0
    ? rs.map(ret => `#buf(32, ${mapInterface(act.interface, toK(ret))})`).join(' ++ ')
    : ".WordStack"
  const output = pass
    && !oog
    && ((act.internal && "VOutput => VOutput")
    || (act.returns
      && ("_ => " + buildReturns(act.returns))
      || act.returnsRaw && "_ => " + act.returnsRaw
      || ".WordStack"))
    || "_ => ?_";

  // STATUSCODE
  const statusCode = (pass && !oog) ? (act.internal ? "?_" : "EVMC_SUCCESS") : "?FAILURE:EndStatusCode"

  // IMPORTS
  const imports = (act.calls || [])
    .map(t => t.replace('.','_'))
    .map(t => pass ? [t + "_pass"] : [t + "_pass"])
    .reduce((a, e) => a.concat(e), [])

  const implName = config.implementations[act.subject].name
  const spec = kjson.renderRule({
    name: act.subject + "_" + act.name,
    fullname: name,
    requires: cond,
    ensures: act.ensures,
    creates: act.creates,
    term: getTerm({
      alias: act.subject,
      wordstack,
      callData: act.callData,
      bin_runtime: config.contracts[implName].bin_runtime,
      oog,
      internal: act.internal,
      output,
      statusCode,
      accounts,
      gasRewrite,
      pc,
      activeAccounts,
      pass,
      isStatic: act.isStatic
    })
  })

  return {
    spec,
    v2n: act.varname2alias,
    act_name: act.act_name,
    imports
  };
}

const buildActs = (config, act_proofs) => {

  const forkPass = (obj, rule) => {
    const gas_json = __getJsonGas(rule.hash)
    let fork_obj = {
      ...obj,
      act: rule.act,
      pass: true,
      rough: false,
      hasGas: true,
      gas: gas_json.map(g => kast.format(g, /*isRaw=*/ true, /*mixFix=*/ true, /*colorize=*/ false, /*forSpec=*/ true))
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
          // const gmt = (name, str) => `requires "data.k"\n\nmodule ${name}\n  imports EVM-DATA\n\n${str}\n\nendmodule`
          // const gas_module = (rule.file_suffix || "")
          //   + rule.ctx
          //     .filter(r => r.file_suffix)
          //     .map(r => r.file_suffix)
          //     .join('\n\n')
          rule.spec = applyWhereBindings(rule.spec, extractWhereBindings(rule.act))
          const _rules = [rule.spec].concat(rule.ctx.map(r => r.spec  + "\n[trusted]\n"))
          const module = kjson.renderModule(_rules, rule.name) // TODO maybe skip this
          const hash   = config.get_proof_hash({name: rule.name, spec: module});
          // const gas_names = (gas_module.length > 0 && [hash] || [])
          rule.module  = kjson.renderModule(_rules, hash);
          rule.hash    = hash;
          rule.status  = getStatus(rule.hash);
          // rule.gas_module = gmt(hash + "GAS", gas_module);

          if(rule.status === 'accept' && rule.rough && __hasGas(rule.hash)) {
            try {
              let pass_name = name.replace('_rough', '');
              act_proofs[pass_name] = forkPass({
                oog: false,
                name: pass_name
              }, rule)
              if((config.oog || []).indexOf(name.replace('_pass_rough','')) > -1) {
                let oog_name = name.replace('_rough', '_oog');
                act_proofs[oog_name] = forkPass({
                  oog: true,
                  name: oog_name
                }, rule);
              }
            } catch(e) {
              console.error("error", rule.hash, name);
              console.error(e);
            }
          }
        }
      })
  }

  return act_proofs;

}

// convenience wrapper to ensure we always have something to iterate over when
// dealing with where bindings
const extractWhereBindings = (act) => {
  if (typeof act.whereBindings === "undefined") return []
  return act.whereBindings
}

// apply where bindings to the spec
const applyWhereBindings = (spec, bindings) => {
  for (const b of bindings) {
    spec = spec.replace(b.from, b.to)
  }
  return spec
}

module.exports = {
  proofCollection,
  makePrelude,
  makeRules,
  getActs,
  parseAct,
  buildAct,
  buildActs,
  caseSplitAct,
  newCleanup,
  makeInterabiExhaustiveness,
  getStatus
}
