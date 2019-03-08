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

  const request$ = xs.merge(
    toTermRequest$,
    toConstraintRequest$,
    // z3feedbackRequest$,
  )

  return {
    pull$: request$
  };
}

module.exports = pull;
