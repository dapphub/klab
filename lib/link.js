const xs = require("xstream").default;
const Constraints = require("./constraints.js");
const { genBehaviour } = require("./behavior.js");

const stateUpdate = {
  "init": (state, msg) => {
    let root_behavior = {
      id: msg.data,
      children: [],
      leaf: false,
      height: 0
    };

    return ({
      ...state,
      initt: msg.data,
      path: [{
        type: "step",
        step: {
          from: "",
          to: msg.data,
          rule: ""
        }
      }],
      status: "running",
      // initialize behaviour computation
      behaviour: {
        ...state.behaviour,
        frontier: [{
          ...root_behavior
        }],
        root: {
          ...root_behavior
        }
      }
    });
  },
  "target": (state, msg) => {
    state.targett = msg.data[0];
    return state;
  },
  "error": (state, msg) => {
    state.error = state.error + "\n" + msg.data
    return state;
  },
  "step": (state, msg) => {
    let from = msg.from;
    let to = msg.to;
    state.edges = Object.assign({}, state.edges);
    state.edges[from] = (state.edges[from] || [])
      .concat([{from, to}])
    // TODO - frontier
    return state;
  },
  // TODO - rename all to blob
  "blob": (state, msg) => {
    let state_ = {
      ...state,
      nodes: {
        ...state.nodes,
        [msg.blobid]: msg.blob
      }
    }
    // Rules
    if(msg.blob.term.att) {
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
  "finished": (state, msg) => {
    return {
      ...state,
      finished: {
        ...state.finished,
        [msg.nodeid]: true
      }
    }
  },
  "crash": (state, msg) => {
    state.crash = Object.assign({}, state.crash, {[msg.data[0]]: true});
    return state;
  },
  // "z3feedbackdata": (state, msg) => {
  //   state.z3feedbackdata = (state.z3feedbackdata || {})
  //   state.z3feedbackdata[msg.z3feedbackid] = msg.data
  //   return state;
  // },
  "z3result": (state, msg) => {
    if(!state.z3feedback[msg.nodeid]) state.z3feedback[msg.nodeid] = []
    state.z3feedback[msg.nodeid].push({
          query:  msg.query,
          result: msg.result,
          rule: msg.rule,
          implication: msg.implication
        })
    return state;
    // return {
    //   ...state,
    //   z3feedback: {
    //     ...state.z3feedback,
    //     [msg.nodeid]: [...(state.z3feedback[msg.nodeid] || []), {
    //       query:  msg.query,
    //       result: msg.result,
    //       rule: msg.rule,
    //       implication: msg.implication
    //     }]
    //   }
    // }
  },
  "status": (state, msg) => {
    state.status = msg.data;
    return state;
  },
  "close": (state, msg) => {
    // state.behaviour.frontier
    //   .forEach(b => b.leaf = true)
    if( state.path.length == 0 ) {
      return {
        ...state,
        status: "fail"
      }
    }
    let behaviour = genBehaviour(state);
    return {
      ...state,
      status: (behaviour.success ? "SUCCESS" : "fail"), // TODO - proof success or failture
      // behaviour: {
      //   ...state.behaviour,
      //   frontier: []
      // }
    }
  },
  "connection": (state, msg) => {

    return {
      ...state,
      connection: {
        ...state.connection,
        status: msg.status
      }
    }
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
