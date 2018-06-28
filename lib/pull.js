const xs = require("xstream").default;
const dropRepeats = require("xstream/extra/dropRepeats").default;
const sampleCombine = require("xstream/extra/sampleCombine").default;
const S = require("./state.js");

const pull = ({K, onion}) => {

  const toConstraintRequest$ = onion.state$
    .filter(state => state && state.path && state.path.length > 0 && (s = state.path[state.path.length - 1]) && !(s.step.to.split("_")[1] in state.nodes))
    .map(state => ({
      type: "getblob",
      proofid: state.config.proofid,
      blobid: state.path[state.path.length - 1].step.to.split("_")[1]
    }))
    .map(s => JSON.stringify(s))
    .compose(dropRepeats())
    .map(s => JSON.parse(s))

  const toTermRequest$ = onion.state$
    .filter(state => state && state.path && state.path.length > 0 && (s = state.path[state.path.length - 1]) && !(s.step.to.split("_")[0] in state.nodes))
    .map(state => ({
      type: "getblob",
      proofid: state.config.proofid,
      blobid: state.path[state.path.length - 1].step.to.split("_")[0]
    }))
    // TODO - simplify
    .map(s => JSON.stringify(s))
    .compose(dropRepeats())
    .map(s => JSON.parse(s))

  const ruleRequest$ = K
    .filter(msg => msg.type == "newrule")
    .compose(sampleCombine(onion.state$))
    .map(([msg, state]) => ({type:"getrule", data: state.config.proofid + " " + msg.data[0]}))

  // this streams a lot
  // TODO - flag or conditional
  const z3feedbackRequest$ = onion.state$
    .filter(state =>
      state.show.z3feedback
      && S.id(state) in state.z3feedback
      && !(state.z3feedback[S.id(state)][0] in (state.z3feedbackdata || {})))
    .map(state => ({
      type:"getz3feedback",
      proofid: state.config.proofid,
      data: state.z3feedback[S.id(state)]
    }))
    .map(s => JSON.stringify(s))
    .compose(dropRepeats())
    .map(s => JSON.parse(s))

  const branchingNodeRequest$ = K
    .filter(msg => msg.type == "specialnode" && msg.data[0] == "branch")
    .compose(sampleCombine(onion.state$))
    .map(([msg, state]) => ({type:"getnode", data: state.config.proofid + " " + msg.data[1]}))

  const request$ = xs.merge(
    toTermRequest$,
    toConstraintRequest$,
    ruleRequest$,
    branchingNodeRequest$,
    z3feedbackRequest$
  );

  return request$;
}

module.exports = pull;
