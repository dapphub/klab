const BN = require('bn.js');
const deepEqual = require('deep-equal');

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
  let subTerm = termList.find( term => term.node == "KApply" && term.label == pathComponent ) || []
  return getKJSONPath(subTerm.args, pathComponents.slice(1))
}

const hex = n => n.toString(16).length % 2 == 0 ? n.toString(16) : "0" + n.toString(16);
const try_hex = (_s, has_prefix = true) => {
  if(/^\d*$/.test(_s.trim())) {
    let n = new BN(_s, 10);
    s = n.toString(16)
    s = (has_prefix ? "0x" : "") + (s.length % 2 == 1 ? "0" : "") + s;
    return s;
  } else {
    return _s.trim()
  }
}

const flatten = (o, label) => {
  return o.label == label
    ? [o.args[0]].concat(flatten(o.args[1], label))
    : [o]
}

const format = (o, isRaw = false) => {
  if(o.label == "#KSequence") {
    o = o.label + "<" + JSON.stringify(o, false, 2) + ">"
  } else if(o.node == 'KVariable') {
    o = o.name;
  } else if(o.node == "KToken") {
    // if(isRaw) {
    //   o = `#token("${o.token}", "${o.sort}")`;
    // } else {
      o = o.token;
    // }
  } else if(o.node == "KApply") {
    let childs = o.args.map(child => format(child, isRaw));
    let infix = {
      "_-Int__INT": " - ",
      "_+Int__INT": " + ",
      "_/Int__INT": " / ",
      "_*Int__INT": " * ",
      "_<=Int__INT": " <=Int ",
      "_>=Int__INT": " >=Int ",
      "_+Word__EVM-DATA": " +W ",
      "_-Word__EVM-DATA": " -W ",
      "_:__EVM-DATA": ":",
      "_==K_": " ==K ",
      "#And" : "\n",
      "intList": " ",
      ".List{\"intList\"}": "",
      ".WordStack_EVM-DATA": ""
    }
    if((!isRaw) && o.label in infix) {
      o = "( " + childs.join(infix[o.label]) + " )"
    } else {
      let cr = childs.reverse();
      let clean_label = o.label.split("_").length > cr.length + 1
        ? o.label.split("_").slice(0, -1).join("_")
        : o.label
      o = "(" + clean_label.replace(/_/g, () => ` ${cr.pop()} `) + ")"
      // o = `\`${o.label}\` ( ${childs.reduce((a, b) => a !== "" ? `${a}, ${b}` : b, "")} )`
    }
  } else {
    return `UNKNOWN<${ JSON.stringify(o) }>`
  }
  return o;
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
  flattenKLabel,
  flattenKLabels,
  flattenNthByteOp,
  match,
  matchList,
  substitute,
  rewriteTopRequire,
  rewriteTop,
  rewriteRequire,
  rewrite
}
