const xs = require("xstream").default;
const dropRepeats = require("xstream/extra/dropRepeats").default;
const sampleCombine = require("xstream/extra/sampleCombine").default;
const flattenSequentially = require("xstream/extra/flattenSequentially").default;
const S = require("./state.js");
// const { genBehaviour } = require("./behavior.js");

const pull = ({K, onion}) => {

  const rstepstate$ = K
    .filter(msg => msg.type === "rstep")
    .compose(sampleCombine(onion.state$))
    .map(([msg, state]) => [msg, state.edges[msg.from], state.rules[msg.rule]])
  const step$ = K
    .filter(msg => msg.type === "step")

  const branch$ = rstepstate$
    .filter(([msg, from_edges]) => !!from_edges)
  const nobranch$ = rstepstate$
    .filter(([msg, from_edges]) => !from_edges)

  // dirty, integrate edge has to come from one source
  const fresh_branch$ = branch$
    .filter(([msg, from_edges]) => from_edges.length == 1)
    // .debug(([msg, state]) => {console.log("int " + msg.from)})
    .map(([msg, from_edges]) => [msg.from, msg.to, from_edges[0].to])
    .map(arr => xs.fromArray(arr))
    .compose(flattenSequentially)
  const old_branch$ = branch$
    .filter(([msg, from_edges]) => from_edges.length > 1)
    .map(([msg, from_edges]) => msg.to)
  const branchNode$ = xs.merge(
    fresh_branch$,
    old_branch$
    )
    .map(id => xs.fromArray(id.split("_")))
    .compose(flattenSequentially)
    .compose(sampleCombine(onion.state$))
    .filter(([id, state]) => !(id in state.nodes))

  const branchNodeRequest$ = branchNode$
    // filter not in nodes
    .map(([id, state]) => ({
      type: "getblob",
      proofid: state.config.proofid,
      blobid: id
    }))

  const pullFinished$ = K
    .filter(msg => msg.type === "finished")
    .map(msg => xs.fromArray(msg.nodeid.split("_")))
    .compose(flattenSequentially)
    .compose(sampleCombine(onion.state$))
    .map(([id, state]) => ({
      type: "getblob",
      proofid: state.config.proofid,
      blobid: id
    }))


  const new_rule$ = rstepstate$
    .filter(([msg, _, msg_rule]) => !msg_rule)
    .map(([msg]) => msg)
  const remember_new_rules$ = new_rule$
    .map(msg => state => ({
      ...state,
      nodes: {
        ...state.nodes,
        [msg.rule]: {}
      }
    }))
  const integrate_step$ = xs.merge(xs.merge(branch$, nobranch$)
    .map(([msg, state]) => msg), step$)
    // .debug(msg => {console.log("add " + msg.from)})
    .map(msg => state => {
      // if(!state.edges[msg.from]) state.edges[msg.from] = [];
      state.edges[msg.from] = (state.edges[msg.from] || []).concat({
        from: msg.from,
        to: msg.to,
        rule: msg.rule
      })
      state.coedges[msg.to] = (state.coedges[msg.to] || []).concat({
        from: msg.from,
        to: msg.to,
        rule: msg.rule
      })
      return state;
    })
      //
      // return ({
      // ...state,
      // edges: {
      //   ...state.edges,
      //   [msg.from]: [
      //     ...(state.edges[msg.from] || []),
      //     {
      //       from: msg.from,
      //       to: msg.to,
      //       rule: msg.rule
      //     }
      //   ]
      // }
      // )}
      // })
      // )
  const integrate_step_behavior$ = rstepstate$
    // .filter(([msg, state]) => !(msg.from in state.edges))
    .map(([msg, s_ ]) => state => {
      let n = state.behaviour.frontier
        .find(node => node.id == msg.from)
      if(!n) {
        let coedges = {};
        Object.keys(state.edges)
          .forEach(from => {
            state.edges[from]
              .forEach(({to}) => {
                coedges[to] = [...(coedges[to] || []), from]
              })
          })
        n = {
          height: 0,
          id: msg.from,
          leaf: false,
          children: []
        };
        let cid = msg.from;
        while (cid != state.initt && !!cid) {
          n.height ++;
          cid = coedges[cid];
        }
      }

      let frontier_ = state.behaviour.frontier
        .filter(e => e != n)

      let n_ = {
        ...n,
        height: n.height + 1,
        id: msg.to
      }

      return {
        ...state,
        behaviour: {
          ...state.behaviour,
          frontier: frontier_.concat(n_)
        }
      };

      // return {
      //   ...state,
      //   behaviour: genBehaviour(state)
      // }
      // let propagate = node => ({
      //   ...node,
      //   id:  node.id == msg.from
      //     && node.children.length == 0
      //     ? msg.to
      //     : node.id,
      //   children: node.children.length >= 0
      //     ? node.children.map(propagate)
      //     : node.children
      // })
      // // console.log(msg);
      // // console.log(state.behaviour.frontier);
      //
      // let node = state.behaviour.frontier
      //   .find(node => node.id == msg.from)
      // let n_ = {
      //   ...node,
      //   id: msg.to,
      //   height: node.height + 1
      // }
      // let frontier_ = state.behaviour.frontier
      //   .filter(el => el.height + 2 > n_.height)
      //   .concat([ n_ ])
      // let height_ = Math.max(
      //   node.height,
      //   state.behaviour.height);
      // return {
      //   ...state,
      //   behaviour: {
      //     ...state.behaviour,
      //     frontier: frontier_,
      //     height: height_,
      //     root: propagate(state.behaviour.root)
      //   },
      // };
    })

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
    branchNodeRequest$,
    pullFinished$
  )

  const delta_state$ = xs.merge(
    integrate_step$,
    integrate_step_behavior$,
    remember_new_rules$
  )

  return {
    pull$: request$,
    remember_pull$: delta_state$
  };
}

module.exports = pull;
