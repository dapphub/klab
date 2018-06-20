const xs = require("xstream").default;
const dropRepeats = require("xstream/extra/dropRepeats").default;
const sampleCombine = require("xstream/extra/sampleCombine").default;

const pull = ({K, onion}) => {

  const toConstraintRequest$ = onion.state$
    .filter(state => state && state.path && state.path.length > 0 && (s = state.path[state.path.length - 1]) && !(s.step.to.split("_")[0] in state.nodes))
    .map(state => state.sid + " " + state.path[state.path.length - 1].step.to.split("_")[0])
    .compose(dropRepeats())
    .map(id => ({type:"getnode", data: id}))

  const toTermRequest$ = onion.state$
    .filter(state => state && state.path && state.path.length > 0 && (s = state.path[state.path.length - 1]) && !(s.step.to.split("_")[1] in state.nodes))
    .map(state => state.sid + " " + state.path[state.path.length - 1].step.to.split("_")[1])
    .compose(dropRepeats())
    .map(id => ({type:"getnode", data: id}))

  const ruleRequest$ = K
    .filter(msg => msg.type == "newrule")
    .compose(sampleCombine(onion.state$))
    .map(([msg, state]) => ({type:"getrule", data: state.sid + " " + msg.data[0]}))

  const circcRequest$ = K
    .filter(msg => msg.type == "circc")
    .compose(sampleCombine(onion.state$))
    .map(([msg, state]) => ({type:"getcircc", data: state.sid + " " + msg.data[0]}))

  // this streams a lot
  // TODO - flag or conditional
  const z3feedbackRequest$ = K
    .filter(msg => msg.type == "z3feedback")
    .compose(sampleCombine(onion.state$))
    .map(([msg, state]) => ({type:"getz3feedback", data: state.sid + " " + msg.data[0] + " " + msg.data[1]}))

  const branchingNodeRequest$ = K
    .filter(msg => msg.type == "specialnode" && msg.data[0] == "branch")
    .compose(sampleCombine(onion.state$))
    .map(([msg, state]) => ({type:"getnode", data: state.sid + " " + msg.data[1]}))

  const request$ = xs.merge(
    toTermRequest$,
    toConstraintRequest$,
    ruleRequest$,
    circcRequest$,
    branchingNodeRequest$,
    z3feedbackRequest$
  );

  return request$;
}

module.exports = pull;
