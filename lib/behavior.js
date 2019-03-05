const C = require("./constraints.js")
const S = require("./state.js")
const kast = require("./kast.js");

// TODO - rewrite with new path in mind
const genBehaviour = (state, init = null) => {
  const FILTER_OOG = state.config && state.config.FILTER_OOG || false;
  var behaviour = {
    id: init || state.initt,
    first: init || state.initt,
    children: [],
    leaf: false,
    height: 0
  }
  var frontier = [
    behaviour
  ]
  var height = 0;
  const potential_leafes = [];
  var last_seen = null;
  const seen_nodes = {};
  const circular = {};

  while (frontier.length > 0) {
    let next_frontier = [];
    height ++;
    frontier.forEach((node, i) => {
      last_seen = node.id;
      let steps = (state.edges[node.id] || []);
      if(!seen_nodes[node.id] && steps.length == 0) {
        potential_leafes.push(node);
      }
      if(!seen_nodes[node.id] && steps.length == 1) {
        node.id = steps[0].to;
        node.endheight = height;
        next_frontier = [...next_frontier, node];
      }
      if(!seen_nodes[node.id] && steps.length > 1) {
        const getDeltaC = step => C.deltaC(state, step)
            .reduce((a, c) => a && andBool(c, a) || c, null)
        const andBool = (a, b) => ({
          node:"KApply",
          label:"_andBool_",
          variable:false,
          arity:2,
          "args": [a, b]
        })
        let children = steps.map((step, i) => {
          let deltaCRaw = getDeltaC(step);
          let deltaC = deltaCRaw
            ? C.clean(kast.format(deltaCRaw).split("\n").filter(s => !!s))
            : ""
            ;
          return {
            index: i,
            head: i + (node.status || ""),
            id: step.to,
            from: step.from,
            first: step.to,
            leaf: false,
            children: [],
            height: height,
            endheight: height,
            step,
            deltaC,
            deltaCRaw
          }
        });
        node.children = children;
        next_frontier = [...next_frontier, ...children]
      }
      if(seen_nodes[last_seen]) {
        circular[last_seen] = true;
        potential_leafes.push(node);
      }
      seen_nodes[last_seen] = true;
    })
    frontier = next_frontier;
  }
  potential_leafes
    .forEach(node => {
      node.leaf = true;
      if(node.id in state.finished) {
        node.status = "✓";
        node.success = true;
        let k = state.nodes[node.id.split("_")[0]];
        if(k) {
          node.oog = kast.get(k, "ethereum.evm.statusCode").label == "EVMC_OUT_OF_GAS_NETWORK"
          node.gas = kast.get(k, "ethereum.evm.callState.gas")
        }
      }
      if(circular[node]) node.status = "circular";
      if(node.id in (state.revert || {})) {
        node.status = "↩";
      }
      if(node.head) node.head = (node.head || node.index) + (node.status ? " " + node.status : "");
    })

  let propagate = key => node => {
    if(node.leaf) return node[key];
    let status = node.children
      .map(propagate(key))
      .reduce((a,b) => a && b, true)
    node[key] = status;
    return status;
  }
  let stateid = S.id(state);
  let propagate_path = (node, {path, active}) => {
    // node.in_history = path.length > 0;
    node.active = active;
    let branching = node.id == stateid;
    node.children
      .forEach((n, i) => {
        if(n.index == path[0]) n.in_history = true;
        n.branching = branching;
        if(branching) {
          n.index = n.index;
        }
        propagate_path(n, {
          path: n.index == path[0] ? path.slice(1) : [],
          active: active && (path[0] == n.index || path.length == 0)
        })
      });
  }

  // TODO - make this functional
  const routing = {}
  const filter = f => node => {
    const localRouting = node.children
      .map((n, i) => [f(n), n.from, i])
      .filter(([tt, from, i]) => tt)
    if(localRouting.length == 1) {
      localRouting
      .forEach(([tt, from, i]) => {
        routing[from] = i
      })
    }
    node.children = node.children
      .map(filter(f))
      .filter(f)
    if(node.children.length == 1) {
      routing[node.children[0].from] = node.children[0].index;
      node.children[0].deltaC = node.deltaC;
      node.children[0].deltaCRaw = node.deltaCRaw;
      node.children[0].index = node.index;
      node.children[0].head = node.head;
      node.children[0].height = node.height;
      node.children[0].from = node.from;
      node.children[0].first = node.first;
      node.children[0].step = node.step;
      return node.children[0];
    } else {
      return node;
    }
  }

  propagate("success")(behaviour)
  propagate("oog")(behaviour)

  if(FILTER_OOG) {
    behaviour = filter(node => !node.oog)(behaviour)
  }

  const clean_path = state.path
    .filter(act => act.type == "branch")
    .map(act => act.branch)

  propagate_path(behaviour, {
    path: clean_path,
    active: true
  })

  behaviour.index = "root";
  if(behaviour.status) behaviour.head = "root " + behaviour.status
  behaviour.in_history = true

  return {behaviour, routing};
}

const getPath = (behaviour, path) => path.length == 0 && behaviour
  || path.length == 1 && path[0].type == 'step' && behaviour
  || getPath(behaviour.children[path[0].branch], path.slice(1))

module.exports = {
  genBehaviour,
  getPath
}
