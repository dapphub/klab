const xs = require("xstream").default;
const dropRepeats = require("xstream/extra/dropRepeats").default;
const sampleCombine = require("xstream/extra/sampleCombine").default;
const flattenSequentially = require("xstream/extra/flattenSequentially").default;
const S = require("./state.js");

const pull = ({K, onion}) => {

  const toConstraintRequest$ = onion.state$
    .filter(state => state.path.length > 0
      && !(S.const_id(state) in state.nodes))
    .map(state => ({
      type: "getblob",
      proofid: state.config.proofid,
      blobid: S.const_id(state)
    }))
    .map(s => JSON.stringify(s))
    .compose(dropRepeats())
    .map(s => JSON.parse(s))

  const toTermRequest$ = onion.state$
    .filter(state => state.path.length > 0
      && !(S.term_id(state) in state.nodes))
    .map(state => ({
      type: "getblob",
      proofid: state.config.proofid,
      blobid: S.term_id(state)
    }))
    // TODO - simplify
    .map(s => JSON.stringify(s))
    .compose(dropRepeats())
    .map(s => JSON.parse(s))



  //TODO - filter on statechange
  const z3feedbackRequest$ = onion.state$
  // trigger only if z3 view is open and view is not empty
    .filter(state =>
      state.show.z3feedback
      && S.id(state) in state.z3feedback)
  // get all z3 data
    .map(state => state.z3feedback[S.id(state)])
    .map(arr => xs.fromArray(arr.reduce((a, e) => a.concat([e.query, e.result, e.rule, e.implication.split("_")[1]]), [])))
    .compose(flattenSequentially)
  // filter for known data
    .compose(sampleCombine(onion.state$))
    .filter(([id, state]) => !(id in state.nodes) && id !== "NORULE" && id !== "IMPLIESTARGET")
  // request data from the server
    .map(([blobid, state]) => ({
      type: "getblob",
      proofid: state.config.proofid,
      blobid: blobid
    }))

  const request$ = xs.merge(
    toTermRequest$,
    toConstraintRequest$,
    z3feedbackRequest$,
  )

  return {
    pull$: request$
  };
}

module.exports = pull;
