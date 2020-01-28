const deepEqual = require('deep-equal');
const clc = require('cli-color');

const KAPPLY         = "KApply"
const KVARIABLE      = "KVariable"
const KTOKEN         = "KToken"
const KSEQUENCE      = "KSequence"
const KREWRITE       = "KRewrite"
const INJECTEDKLABEL = "InjectedKLabel"

const KApply = (klabel, args) => {
  return { "node"     : KAPPLY
         , "label"    : klabel
         , "arity"    : args.length
         , "variable" : false
         , "args"     : args
         }
}

const KVariable = (name) => {
  return { "node" : KVARIABLE
         , "name" : name
         }
}

const KToken = (sort, token) => {
  return { "node"  : KTOKEN
         , "sort"  : sort
         , "token" : token
         }
}

const KInt = i => {
  return KToken("Int", JSON.stringify(i))
}

const isIntToken = k => {
  return k.node == KTOKEN && k.sort == "Int"
}

// `parseInt` is risky, will return trash for too large numbers
// look into bn.js or big.js for bignum support
const fromIntToken = k => {
  return parseInt(k.token)
}

const isKLabel = (kLabel, k) => {
  return k.node == KAPPLY && k.label == kLabel
}

const joinMatches = (match1, match2) => {
  if (match1 == null || match2 == null) {
    return null
  }
  var newMatch = match1
  for (var k in match2) {
    if (k in match1 && (! deepEqual(match1[k], match2[k]))) {
      return null
    } else {
      newMatch[k] = match2[k]
    }
  }
  return newMatch
}


const antimatch = (pattern, term, path = []) => {
  if (pattern.node == KVARIABLE) {
    return [];
  } else if (pattern.node == KAPPLY && term.node == KAPPLY && pattern.label == term.label) {
    return antimatchList(pattern.args, term.args, path.concat(pattern.label))
  } else if (pattern.node == KTOKEN && deepEqual(pattern, term)) {
    return [];
  } else {
    return [
      [path, format(pattern), format(term)]
    ];
  }
}


const antimatchList = (patterns, terms, path) => {
  if (patterns.length != terms.length) {
    return []; // TODO
  } else if (patterns.length == 0) {
    return [];
  } else {
    return patterns
      .map((p,i) => antimatch(p, terms[i], path))
      .reduce((a, e) => a.concat(e), [])
  }
}

const match = (pattern, term) => {
  if (pattern.node == KVARIABLE) {
    return { [pattern.name] : term }
  } else if (pattern.node == KAPPLY && term.node == KAPPLY && pattern.label == term.label) {
    return matchList(pattern.args, term.args)
  } else if (pattern.node == KTOKEN && deepEqual(pattern, term)) {
    return {}
  } else {
    return null
  }
}

const matchList = (patterns, terms) => {
  if (patterns.length != terms.length) {
    return null
  } else if (patterns.length == 0) {
    return {}
  } else {
    return joinMatches(match(patterns[0], terms[0]), matchList(patterns.slice(1), terms.slice(1)))
  }
}

const visitChildren = (k, f) => {
  if (k.node == KAPPLY) {
    return KApply(k.label, k.args.map(f))
  } else {
    return k
  }
}

const visitBottomUp = (k, f) => {
  return f(visitChildren(k, (arg => visitBottomUp(arg, f))))
}

const visitTopDown = (k, f) => {
  return visitChildren(f(k), (arg => visitTopDown(arg, f)))
}

const omitArgs = k => {
  if (k.node == KAPPLY) {
    return KApply(k.label, [])
  } else {
    return k
  }
}

const substitute = (k, subst) => {
  const _substitute = kVar => {
    return kVar.node == KVARIABLE
        && subst[kVar.name]
        || kVar
  }
  return visitBottomUp(k, _substitute)
}

const rewriteTopRequire = (lhs, rhs, req) => {
  return (k => { var lhsMatch = match(lhs, k)
                 if (lhsMatch == null || (! req(lhsMatch))) return k
                 return substitute(rhs, lhsMatch)
               }
         )
}

const rewriteTop = (lhs, rhs) => {
  return (k => rewriteTopRequire(lhs, rhs, (subst => true))(k))
}

const rewriteRequire = (lhs, rhs, req) => {
  return (k => visitBottomUp(k, rewriteTopRequire(lhs, rhs, req)))
}

const rewrite = (lhs, rhs) => {
  return (k => visitBottomUp(k, rewriteTop(lhs, rhs)))
}

// TODO: rewrite using matching/substitution mechanisms
const flattenKLabel = klabel => {
  const gatherChildren = (kChild => isKLabel(klabel, kChild)
                                 && kChild.args
                                 || [ kChild ]
                         )
  return (k => isKLabel(klabel, k)
            && KApply(k.label, [].concat.apply([], k.args.map(gatherChildren)))
            || k
         )
}

const flattenKLabels = klabels => {
  const buildFlattener = (ks => ks.length == 0
                             && (k => k)
                             || (k => flattenKLabel(ks[0])(buildFlattener(klabels.slice(1))(k)))
                         )
  return (k => visitBottomUp(k, buildFlattener(klabels)))
}

// Perhaps below should be turned into the following:

// rule nthbyteof(vName, start1, width)             : rest
//   => nthbyteof(vName, start1, start1 + 1, width) : rest
//   requires isInt(start1)

// rule nthbyteof(vName, start1, end1, width) : nthbyteof(vName, start2, end2, width) : rest
//   => nthbyteof(vName, start1, end2, width)                                         : rest
//   requires isInt(start1) andBool isInt(end1) andBool isInt(start2) andBool isInt(end2) andBool end1 + 1 == start2
const flattenNthByteOp = k => {
  const nthByteLOf = (start => KApply("nthbytelof", [ KVariable("vName"), start, start, KVariable("width") ]))
  const nthByteOf = (start => KApply("nthbyteof", [ KVariable("vName") , start, KVariable("width") ]))
  const wordStack = ((head, tail) => KApply("_:__EVM-DATA", [ head , tail ]))
  return rewriteRequire( wordStack(nthByteOf(KVariable("start1")), wordStack(nthByteOf(KVariable("start2")), KVariable("rest")))
                       , wordStack(nthByteOf(KVariable("start1")), KVariable("rest"))
                       , (subst => isIntToken(subst["start1"]) && isIntToken(subst["start2"])
                                && fromIntToken(subst["start1"]) + 1 == fromIntToken(subst["start2"])
                         )
                       )(k)
  // return rewrite(
  //   nthByteOf(KVariable("start1")),
  //   nthByteLOf(KVariable("start1"))
  // )(k)
}

const getKJSONPath = (termList, pathComponents) => {
  if (termList == undefined)      return {}
  if (pathComponents.length == 0) return termList
  let pathComponent = pathComponents[0]
  let subTerm = termList.find( term => term.node == KAPPLY && term.label == pathComponent ) || []
  return getKJSONPath(subTerm.args, pathComponents.slice(1))
}

const flatten = (o, label) => {
  return o.label == label
    ? [o.args[0]].concat(flatten(o.args[1], label))
    : [o]
}

const omitCells = (o, cellNames) => {
  if(o.node == KAPPLY) {
    if (o.label[0] == "<" && o.label[o.label.length - 1] == ">" && cellNames.includes(o.label.slice(1, o.label.length - 1))) {
      return KToken("Cell", o.label + "(OMITTED)")
    }
    let childs = o.args.map(child => omitCells(child, cellNames));
    return KApply(o.label, childs)
  }
  return o;
}

const format = (o, isRaw = false, mixFix = false, colorize = false) => {
  if(o.node == KVARIABLE) {
    o = (/^(.*)_\d+\:\w+$/).test(o.originalName)
      ? (/^(.*)_\d+\:\w+$/).exec(o.originalName)[1]
      : o.originalName
      ;
  } else if(o.node == KTOKEN) {
    const pt = {
      "115792089237316195423570985008687907853269984665640564039457584007913129639936": "pow256",
      "115792089237316195423570985008687907853269984665640564039457584007913129639935": "maxUInt256",
      "-57896044618658097711785492504343953926634992332820282019728792003956564819968": "minSInt256",
      "57896044618658097711785492504343953926634992332820282019728792003956564819967": "maxSInt256",
      "57896044618658097711785492504343953926634992332820282019728792003956564819968": "pow255",
      "26959946667150639794667015087019630673637144422540572481103610249216": "pow224",
      "26959946667150639794667015087019630673637144422540572481103610249215": "maxUInt224",
      "411376139330301510538742295639337626245683966408394965837152256": "pow208",
      "95780971304118053647396689196894323976171195136475136": "pow176",
      "1461501637330902918203684832716283019655932542976": "pow160",
      "1461501637330902918203684832716283019655932542975": "maxUInt160",
      "340282366920938463463374607431768211456": "pow128",
      "340282366920938463463374607431768211455": "maxUInt128",
      "5192296858534827628530496329220096": "pow112",
      "5192296858534827628530496329220095": "maxUInt112",
      "18446744073709551616": "pow64",
      "18446744073709551615": "maxUInt64",
      "281474976710656": "pow48",
      "281474976710655": "maxUInt48",
      "4294967296": "pow32",
      "4294967295": "maxUInt32",
      "1000000000000000000000000000": "#Ray"
    }
    o = o.token in pt
        ? (colorize && clc.bold(pt[o.token]) || pt[o.token])
        : o.token
  }
  else if(o.node == KAPPLY) {
    let associative = {
      "intList": true
    }
    let assoc = o.label in associative;
    let tokenMap = {
      "keccakIntList": "keccak",
      ".IntList": "",
      "intList" : "",
      ".List{\"intList\"}": "",
      ".WordStack_EVM-DATA": "",
    }
    o.label = o.label in tokenMap ? tokenMap[o.label] : o.label;
    const childs = o.args.map(child => format(child, isRaw, mixFix))
        .filter(s => !(s == ''));
    if (childs.length == 0) {
      return o.label;
    }
    let infix = {
      "_+Int_"             : " + "   ,
      "_-Int_"             : " - "   ,
      "_*Int_"             : " * "   ,
      "_/Int_"             : " / "   ,
      "_modInt_"           : " mod " ,
      "_<Int_"             : " < "   ,
      "_<=Int_"            : " <= "  ,
      "_>Int_"             : " > "   ,
      "_>=Int_"            : " >= "  ,
      "_&Int_"             : " & "   ,
      "_|Int_"             : " | "   ,
      "_+Int__INT-COMMON"  : " + "   ,
      "_-Int__INT-COMMON"  : " - "   ,
      "_*Int__INT-COMMON"  : " * "   ,
      "_/Int__INT-COMMON"  : " / "   ,
      "_modInt__INT-COMMON": " mod " ,
      "_<Int__INT-COMMON"  : " < "   ,
      "_<=Int__INT-COMMON" : " <= "  ,
      "_>Int__INT-COMMON"  : " > "   ,
      "_>=Int__INT-COMMON" : " >= "  ,
      "_|Int__INT-COMMON"  : " | "   ,
      "_&Int__INT-COMMON"  : " & "   ,

      "_++_WS"             : " ++ "  ,

      "_+Word__EVM-DATA"   : " +W "  ,
      "_-Word__EVM-DATA"   : " -W "  ,
      "_*Word__EVM-DATA"   : " *W "  ,
      "_/Word__EVM-DATA"   : " /W "  ,
      "_%Word__EVM-DATA"   : " %W "  ,
      "_^Word__EVM-DATA"   : " ^W "  ,

      "_+Map__RULES"       : " +M "  ,

      "_andBool_"          : " AND " ,
      "_orBool__BOOL"      : " OR "  ,
      "_:__EVM-DATA"       : " : "   ,
      "_==K_"              : " ==K " ,
      "#And"               : "\n"    ,
      "#KSequence"         : "\n~> " ,
      "_Map_"              : "\n"    ,
      "_|->_"              : " |-> "
    }
    if((!isRaw) && o.label in infix) {
      o = "(" + childs.join(infix[o.label]) + ")"
    } else if (o.label[0] == "<" && o.label[o.label.length - 1] == ">") {
      let indentedChildren = childs.map(gChild => gChild.split("\n").join("\n  "))
      o = o.label + "\n  " + indentedChildren.join("\n  ") + "\n</" + o.label.slice(1)
    } else if (mixFix && o.label.indexOf('_') > -1) {
      let cr = childs.reverse();
      let clean_label = o.label.split("_").length > cr.length + 1
        ? o.label.split("_").slice(0, -1).join("_")
        : o.label
      o = "(" + clean_label.replace(/_/g, () => ` ${cr.pop()} `) + ")"
      // o = `\`${o.label}\` ( ${childs.reduce((a, b) => a !== "" ? `${a}, ${b}` : b, "")} )`
    } else {
      o = o.label + (assoc ? childs.join(", ") : "(" + childs.join(", ") + ")")
    // } else {
    //   o = o.label;
    }
  } else {
    return `UNKNOWN<${ JSON.stringify(o) }>`
  }
  return o;
}

const prettify = str => {
  if(typeof str === "string") {
    str = str.replace(/ \.IntList/g, "");
    str = str.replace(/ _\-Int__INT /g, " - ");
    str = str.replace(/ _\+Int__INT /g, " + ");
    str = str.replace(/ _\/Int__INT /g, " / ");
    str = str.replace(/ _\*Int__INT /g, " * ");
    str = str.replace(/115792089237316195423570985008687907853269984665640564039457584007913129639935/g, "maxUInt256")
  }
  return str;
}

const get = (term, path) => {
  let p = path
    .split(".")
    .map(s => `<${s}>`)
  let o = getKJSONPath(term.args, p)[0]
  return o;
}

const getO = (term, o) => {
  Object.keys(o)
    .forEach(key => {
      o[key] = get(term, o[key]);
    })
  return o
}

module.exports = {
  get,
  getO,
  format,
  flatten,
  KAPPLY,
  KVARIABLE,
  KTOKEN,
  KSEQUENCE,
  KREWRITE,
  INJECTEDKLABEL,
  KApply,
  KVariable,
  KToken,
  KInt,
  isIntToken,
  visitChildren,
  visitBottomUp,
  visitTopDown,
  omitArgs,
  omitCells,
  flattenKLabel,
  flattenKLabels,
  flattenNthByteOp,
  match,
  antimatch,
  matchList,
  substitute,
  rewriteTopRequire,
  rewriteTop,
  rewriteRequire,
  rewrite,
  prettify
}
