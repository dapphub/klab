const term_id = state => {
  return state.path[state.path.length - 1].step.to.split("_")[0];
}
const const_id = state => {
  return state.path[state.path.length - 1].step.to.split("_")[0];
}

const id = state => {
  return state.path[state.path.length - 1].step.to
}

const steps = state => {
  let id = term_id(state);
  return state.edges[id];
}


module.exports = {
  term_id,
  const_id,
  id,
  steps
}
