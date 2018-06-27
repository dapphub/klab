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
  let o = getKJSONPath(term.args, p)[0].token;
  return o;
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
  // console.log(ast);
  if (ast.label == "#And") {
    return ast.args
      .map( arg => arg.token.trim() )
      .filter(s => s != "")
      .join("\n")
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
