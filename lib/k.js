const getKJSONPath = (termList, pathComponents) => {
  if (pathComponents.length == 0) return termList
  let pathComponent = pathComponents[0]
  let subTerm = termList.find( term => term.node == "KApply" && term.label == pathComponent )
  return getKJSONPath(subTerm.args, pathComponents.slice(1))
}

const get = (term, path) => {
  let p = path
    .split(".")
    .map(s => `<${s}>`)
  let o = getKJSONPath(term.args, p)[0];
  if(o.arity == 0) {
    o = o.label;
  } else if(o.label == "#KSequence") {
    o = JSON.stringify(o, false, 2)
  } else {
    o = o.token;
  }
  return o;
}

const getO = (term, o) => {
  Object.keys(o)
    .forEach(key => {
      o[key] = get(term, o[key]);
    })
  return o
}

const format = ast => {

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

module.exports = {
  get,
  getO,
  formatConstraint
}
