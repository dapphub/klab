const xs = require("xstream").default;
const _ = require("lodash");
const Constraints = require("./constraints.js");

const stateUpdate = {
  "sid": (state, msg) => {
    state.sid = msg.data
    return state;
  },
  "init": (state, msg) => {
    state.initt = msg.data[0]
    state.path = [{
      type: "step",
      step: {
        from: "",
        to: msg.data[0],
        rule: "",
        step: 0
      }
    }];
    return state;
  },
    //todo: probably "finished"
  "targett": (state, msg) => {
    state.targett = msg.data[0];
    return state;
  },
  "error": (state, msg) => {
    state.error = state.error + "\n" + msg.data
    return state;
  },
    "step": (state, msg) => {
     console.log('step')
     console.log(msg.data[0])
     console.log(msg.data[1])
     console.log(msg.data[2])
    let step = msg.data[0];
    let from = msg.data[1];
    let to = msg.data[2];
    state.edges = Object.assign({}, state.edges);
    state.edges[from] = (state.edges[from] || [])
      .concat([{step, from, to}])
    state.steps = Math.max(step, state.steps);
    return state;
  },
  "rstep": (state, msg) => {
    console.log('step')
    console.log(msg.data[0])
    console.log(msg.data[1])
    console.log(msg.data[2])
    let step = msg.data[0];
    let from = msg.data[1];
    let to = msg.data[2];
    let rule = msg.data[3];
    state.edges = Object.assign({}, state.edges);
    state.edges[from] = (state.edges[from] || [])
      .concat([{step, from, to, rule}])
    state.steps = Math.max(step, state.steps);
    // TODO - build behaviour here
    return state;
  },
    "node": (state, msg) => {
//        console.log('node! : ' + msg.toString())
    state.nodes = Object.assign({}, state.nodes);
    state.nodes[msg.data.id] = msg.data.node;
    return state;
  },
  "rule": (state, msg) => {
    state.rules = Object.assign({}, state.rules);
    state.rules[msg.data.id] = msg.data.rule;
    return state;
  },
  "normalnode": (state, msg) => {
    state.normalnodes = Object.assign({}, state.normalnodes, {[msg.data[0]]: true});
    return state;
  },
  "specialnode": (state, msg) => {
    state[msg.data[0]] = Object.assign({}, state[msg.data[0]], {[msg.data[1]]: true});
    return state;
  },
  "crash": (state, msg) => {
    state.crash = Object.assign({}, state.crash, {[msg.data[0]]: true});
    return state;
  },
  "circcdata": (state, msg) => {
    let circc = JSON.parse(msg.data.circc);
    state.circc = Object.assign({}, state.circc);
    let id = circc.term;
    if(!(id in state.circc)) state.circc[id] = [];
    let rhs = Constraints.clean(circc.rhs)//.replace("\\n", "\n").split("\n").map(s => s.trim()).join("\n");
    state.circc[id] = _.uniq([rhs].concat(state.circc[id]));
    return state;
  },
  "z3feedbackdata": (state, msg) => {
    state.z3feedback = (state.z3feedback || {})
    state.z3feedback[msg.nodeId] = msg.data
    return state;
  },
  "z3feedback": (state, msg) => {
    let term_id = msg.data[0];
    state.z3feedback = Object.assign({}, state.z3feedback);
    if(!(term_id in state.z3feedback)) state.z3feedback[term_id] = {};
    let z3id = msg.data[1];
    state.z3feedback[term_id][z3id] = {};
    return state;
  }

}



module.exports = (K) => {

  // update state based on K messeges
  const link$ = K
    .filter(msg => msg.type in stateUpdate)
    .map(msg => state =>
      Object.assign({}, stateUpdate[msg.type](state, msg))
    )

  return link$;
}
