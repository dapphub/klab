const stateUpdate = {
  "blob": (state, msg) => {
    let state_ = {
      ...state,
      nodes: {
        ...state.nodes,
        [msg.blobid]: msg.blob
      }
    }
    // Rules
    if(msg.blob.att) {
      let tags = (msg.blob.term.att
        .match(/tag\(([^\)]*)\)/g) || [])
        .map(s => s.slice(4, -1))
      if(tags.indexOf("step") > -1) {
        state_.steps = {
          ...state_.steps,
          [msg.blobid]: true
        };
      }
      state_.rules[msg.blobid] = msg.rule;
    }
    return state_;
  },
  "z3result": (state, msg) => {
    if(!state.z3feedback[msg.nodeid]) state.z3feedback[msg.nodeid] = []
    state.z3feedback[msg.nodeid].push({
          query:  msg.query,
          result: msg.result,
          rule: msg.rule,
          implication: msg.implication
        })
    return state;
  }
}



module.exports = (K) => {

  // update state based on K messeges
  const link$ = K
    .filter(msg => msg.type in stateUpdate)
    .map(msg => state => {
      return Object.assign({}, stateUpdate[msg.type](state, msg))
    })

  return link$;
}
