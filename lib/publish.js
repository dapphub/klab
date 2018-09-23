const fs = require("fs");
const path = require("path");
const {
  testPath,
  read
} = require("../lib/util.js");
const pure = require("../lib/pure.js");

const KLAB_WD_PATH = path.join(process.env.TMPDIR, "klab");
if(!testPath(KLAB_WD_PATH)) fs.mkdirSync(KLAB_WD_PATH);
const KLAB_EVMS_PATH = process.env.KLAB_EVMS_PATH;
const KLAB_K_PATH  = process.env.KLAB_K_PATH
                || path.join(KLAB_EVMS_PATH, "./.build/k");

const prune_edges = proofid => {
  console.log(`pruning ${proofid}`);

  const logs = read(path.join(KLAB_WD_PATH, proofid + ".log"))
    .split("\n")
    .filter(l => !!l)
  // .map(l => l.split(" "))

  const initt = logs
    .find(l => l.split(" ")[1] == "init")
    .split(" ")[3]
  const targett = logs
    .find(l => l.split(" ")[1] == "target")
    .split(" ")[3]

  const edges_logs = logs
    .filter(l => ["rstep", "step", "srstep"].indexOf(l.split(" ")[1]) > -1)

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
    .filter(l => l.split(" ")[1] == "rule")
    .map(l => l.split(" ")[3])
    .reduce((a, ruleid) => {
      if(!a[ruleid]) a[ruleid] = JSON.parse(read(path.join(KLAB_WD_PATH, "blobs", ruleid + ".json")));
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

  while(state.frontier.length > 0) {
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

  const pruned_edges = state.edges
    .reduce((a, e) => {
      a[e.from] = (a[e.from] || []).concat({
        from: e.from,
        to: e.to
      })
      return a;
    }, {})

  const old_edges_length = edges_logs.length;
  const pruned_edges_length = state.edges.length;
  const reduction_ratio = Math.floor((pruned_edges_length / old_edges_length) * 100)
  console.log(`Reduced to ${reduction_ratio}%`);

  return {
    pruned_edges,
    initt,
    targett
  }
}

module.exports = {
  prune_edges
};
