// const fs = require("fs");
const path = require("path");
const {
  testPath,
  read
} = require("../lib/util.js");
const pure = require("../lib/pure.js");
const kast = require("./kast.js");

// TODO - throw error if TMPDIR is not set
const KLAB_WD_PATH = path.join(process.env.TMPDIR, "klab");
// if(!testPath(KLAB_WD_PATH)) fs.mkdirSync(KLAB_WD_PATH);
const KLAB_EVMS_PATH = process.env.KLAB_EVMS_PATH;
const KLAB_K_PATH  = process.env.KLAB_K_PATH
                || path.join(KLAB_EVMS_PATH, "./.build/k");

const prune_edges = (proofid, prune = true) => {
  // console.log(`pruning ${proofid}`);

  const logs = read(path.join(KLAB_WD_PATH, proofid + ".log"))
    .split("\n")
    .filter(l => !!l)
  // .map(l => l.split(" "))

  const initt = logs
    .find(l => l.split(" ")[1] == "REACHINIT")
    .split(" ")[2]
  const targett = logs
    .find(l => l.split(" ")[1] == "REACHTARGET")
    .split(" ")[2]

  const finished = logs
    .filter(l => l.split(" ")[1] == "REACHPROVED")
    .map(l => l.split(" ")[2])

  const edges_logs = logs
    .filter(l => ["RULEAPPLICATION"].indexOf(l.split(" ")[1]) > -1)

  const edges = edges_logs
    .map(l => pure({data: l}))
    .reduce((a, e) => {
      a[e.from] = [...(a[e.from] || []), {
        from: e.from,
        to: e.to,
        rule: e.rule
      }]
      return a;
    }, {})

  const rules = logs
    .filter(l => l.split(" ")[1] == "RULE")
    .map(l => l.split(" ")[2])
    .reduce((a, ruleid) => {
      if(!a[ruleid]) a[ruleid] = JSON.parse(read(path.join(KLAB_WD_PATH, proofid + "_blobs", ruleid + ".json")));
      return a;
    }, {})

  const steps = Object.keys(rules)
    .filter(ruleid => {
      let tags = (rules[ruleid].term.att
        .match(/tag\(([^\)]*)\)/g) || [])
        .map(s => s.slice(4, -1))
      return tags.indexOf("step") > -1
    })

  // propagate steps, prune non-steps
  const isStep     = edge => steps.indexOf(edge.rule) > -1
  const isRelevant = edge => isStep(edge) || edge.to == targett;

  const state = {
    edges: [],
    frontier: [
      {
        from: initt,
        to: initt
      }
    ]
  }

  while(prune && state.frontier.length > 0) {
    // console.log("frontier", state.frontier);
    let frontier_ = state.frontier
      .filter(edge => edges[edge.to])
      .map(edge => edges[edge.to].map(e => ({
        ...edge,
        to: e.to,
        rule: e.rule
      })))
      .reduce((a, e) => a.concat(e), [])
    // console.log("frontier_", frontier_);
    let steps_ = frontier_
      .filter(edge => isRelevant(edge))
    // console.log("steps", steps_);
    let nonsteps_ = frontier_
      .filter(edge => !isRelevant(edge))
    // console.log("nonsteps", nonsteps_);
    state.edges = state.edges.concat(steps_);
    state.frontier = steps_
      .map(edge => ({
        from: edge.to,
        to: edge.to
      }))
      .concat(nonsteps_)
  }

  var pruned_edges = state.edges
    .reduce((a, e) => {
      a[e.from] = (a[e.from] || []).concat({
        from: e.from,
        to: e.to
      })
      return a;
    }, {})

  if(!prune) {
    pruned_edges = edges;
  }

  const old_edges_length = edges_logs.length;
  const pruned_edges_length = state.edges.length;
  const reduction_ratio = Math.floor((pruned_edges_length / old_edges_length) * 100)
  // console.log(`Reduced edges to ${reduction_ratio}%`);

  return {
    finished,
    pruned_edges,
    initt,
    targett
  }
}

const getInfo = k => ({
  pc: kast.format( kast.get(k, "ethereum.evm.callState.pc") ),
  call_id: kast.format( kast.get(k, "ethereum.evm.callState.id") ),
  acc_map: kast.get(k, "ethereum.network.accounts"),
  stack_kast: kast.flattenNthByteOp(kast.get(k, "ethereum.evm.callState.wordStack"))
})

const buildDisplayInfo = (k, config) => {

  const {
    pc,
    call_id,
    acc_map,
    stack_kast
  } = getInfo(k);

  let stack_flatt_kast = kast.flatten(stack_kast, "_:__EVM-DATA")
    .filter(c => c.label !== ".WordStack_EVM-DATA")
    .map(c => kast.format(c))


  let acc = acc_map
    .args.find(o => {
      return o.args
        && o.args[0]
        && o.args[0].args
        && o.args[0].args[0]
        && kast.format(o.args[0].args[0]) == call_id
    })
    .args[1]
    //  let storage = kast.flattenNthByteOp(kast.get(acc, "storage"))
    let storage_object = kast.prettify(kast.get(acc, "storage").token);
  // console.log(JSON.stringify(storage.args, false, 2));
/*    let storage_object = (storage.args || [])
    .filter(m => m.args)
    .map(m => kast.format(m.args))
    .reduce((a, store) => ({...a, store}), {})
*/
  //JSON.stringify(storage, false, 2)

  return {
    pc,
    stack_flatt_kast,
    storage_object,
    call_id
  };
}

module.exports = {
  prune_edges,
  buildDisplayInfo
};
