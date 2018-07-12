
const getKJSONPath = (termList, pathComponents) => {
  if (pathComponents.length == 0) return termList
  let pathComponent = pathComponents[0]
  let subTerm = termList.find( term => term.node == "KApply" && term.label == pathComponent )
  return getKJSONPath(subTerm.args, pathComponents.slice(1))
}

const format = o => {
  if (o.arity == 0) {
    o = o.label;
  } else if(o.label == "#KSequence") {
    o = JSON.stringify(o, false, 2)
  } else if(o.node == 'KVariable') {
    o = o.name;
  } else if("token" in o) {
    o = o.token;
  } else {
    // o = JSON.stringify(o, false, 2)
  }
  return o;
}

const get = (term, path) => {
  let p = path
    .split(".")
    .map(s => `<${s}>`)
  let o = getKJSONPath(term.args, p)[0];
  return format(o)
}

const getO = (term, o) => {
  Object.keys(o)
    .forEach(key => {
      o[key] = get(term, o[key]);
    })
  return o
}

const formatConstraint = ast => {
  ast = ast.term;
  if (ast.label == "#And") {
    return ast.args
      .map( arg => arg.token.trim() )
      .filter(s => s != "")
      .join("\n")
  } else if(ast.label == "_==K_") {
    return ast.args[0].token;
  } else if (ast.arity == 0) {
    return ast.label
  } else {
    return ast.token
  }
}

const foldMap = map => {
  return map
    .filter(e => e.node !== "KVariable")
    .reduce((o, entry) => ({...o, [entry.args[0].token]: format(entry.args[1]) }), {})
}

module.exports = {
  get,
  getO,
  formatConstraint,
  foldMap
}
