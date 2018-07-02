const xs = require("xstream").default;
const dropRepeats = require("xstream/extra/dropRepeats").default;
const sampleCombine = require("xstream/extra/sampleCombine").default;
const flattenSequentially = require("xstream/extra/flattenSequentially").default;
const S = require("./state.js");

const pull = ({K, onion}) => {

  const rstepstate$ = K
    .filter(msg => msg.type === "rstep")
    .compose(sampleCombine(onion.state$))

  const branch$ = rstepstate$
    .filter(([msg, state]) => (msg.from in state.edges))
  const fresh_branch$ = branch$
    .filter(([msg, state]) => state.edges[msg.from].length == 1)
    .map(([msg, state]) => xs.fromArray([msg.from, msg.to, state.edges[msg.from][0].to]))
    .compose(flattenSequentially)
  const old_branch$ = branch$
    .filter(([msg, state]) => state.edges[msg.from].length > 1)
    .map(([msg, state]) => msg.to)
  const branchNodeRequest$ = xs.merge(
    fresh_branch$,
    old_branch$
    )
    .map(id => xs.fromArray(id.split("_")))
    .compose(flattenSequentially)
    .compose(sampleCombine(onion.state$))
    .filter(([id, state]) => !(id in state.nodes))
    // filter not in nodes
    .map(([id, state]) => ({
      type: "getblob",
      proofid: state.config.proofid,
      blobid: id
    }))

  const new_rule$ = rstepstate$
    .filter(([msg, state]) => !(msg.rule in state.nodes))
    .map(([msg, state]) => msg)
  const remember_new_rules$ = new_rule$
    .map(msg => state => ({
      ...state,
      nodes: {
        ...state.nodes,
        [msg.rule]: {}
      }
    }))
  const integrate_step$ = rstepstate$
    .map(([msg, state]) => msg)
    .map(msg => state => ({
      ...state,
      edges: {
        ...state.edges,
        [msg.from]: [
          ...(state.edges[msg.from] || []),
          {
            from: msg.from,
            to: msg.to,
            rule: msg.rule
          }
        ]
      }
    }))

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

  const ruleRequest$ = new_rule$
    .compose(sampleCombine(onion.state$))
    .map(([msg, state]) => ({
      type: "getblob",
      blobid: msg.rule,
      proofid: state.config.proofid
    }))

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

  // const branchingNodeRequest$ = K
  //   .filter(msg => msg.type == "specialnode" && msg.data[0] == "branch")
  //   .compose(sampleCombine(onion.state$))
  //   .map(([msg, state]) => ({type:"getnode", data: state.config.proofid + " " + msg.data[1]}))

  const request$ = xs.merge(
    toTermRequest$,
    toConstraintRequest$,
    ruleRequest$,
    // branchingNodeRequest$,
    z3feedbackRequest$,
    branchNodeRequest$
  )

  const delta_state$ = xs.merge(
    integrate_step$,
    remember_new_rules$
  )

  return {
    pull$: request$,
    remember_pull$: delta_state$
  };
}

module.exports = pull;
