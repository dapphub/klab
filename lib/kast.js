const BN = require('bn.js');

const KAPPLY         = "KApply"
const KSEQUENCE      = "KSequence"
const KVARIABLE      = "KVariable"
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

const getKJSONPath = (termList, pathComponents) => {
  if (pathComponents.length == 0) return termList
  let pathComponent = pathComponents[0]
  let subTerm = termList.find( term => term.node == "KApply" && term.label == pathComponent )
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

const format = o => {
  if(o.label == "#KSequence") {
    o = o.label + "<" + JSON.stringify(o, false, 2) + ">"
  } else if(o.node == 'KVariable') {
    o = o.name;
  } else if(o.node == "KToken") {
    o = o.token;
  } else if(o.node == "KApply") {
    let childs = o.args.map(child => format(child));
    let infix = {
      "_-Int__INT": " - ",
      "_+Int__INT": " + ",
      "_+Word__EVM-DATA": " +W ",
      "_-Word__EVM-DATA": " -W ",
      "_:__EVM-DATA": ":",
      "_==K_": " ==K ",
      "#And" : "\n"
    }
    if(o.label in infix) {
      o = childs.join(infix[o.label])
    } else {
      o = o.label + "( " + childs.join(",,") + " )"
    }
  } else {
    return `UNKNOWN<${ JSON.stringify(o) }>`
    // o = JSON.stringify(o, false, 2)
  }
  return o;
}

const get = (term, path) => {
  let p = path
    .split(".")
    .map(s => `<${s}>`)
  let o = getKJSONPath(term.args, p)[0];
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
  omitArgs,
  visitChildren,
  visitBottomUp,
  visitTopDown,
  KApply
}
