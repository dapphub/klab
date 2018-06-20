const term_id = state => {
  return state.path[state.path.length - 1].step.to.split("_")[0];
}

const steps = state => {
  let id = term_id(state);
  return state.edges[id];
}


module.exports = {
  term_id,
  steps
}
