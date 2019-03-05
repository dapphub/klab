const Constraints = require("./constraints.js");
const {format} = require("./kast.js");

const term_id = state => {
  return state.path[state.path.length - 1].step.to.split("_")[0];
}
const const_id = state => {
  return state.path[state.path.length - 1].step.to.split("_")[1];
}
const rule = state => {
  return state.path[state.path.length - 1].step.rule
}

const id = state => {
  return state.path[state.path.length - 1].step.to
}

const constr = state => {
  let id = const_id(state);
  let cs = state.nodes[id].args
    .map(format)
  return Constraints.clean(cs)
}
const steps = state => {
  let _id = id(state);
  return state.edges[_id];
}


module.exports = {
  rule,
  term_id,
  const_id,
  const: constr,
  id,
  steps
}
