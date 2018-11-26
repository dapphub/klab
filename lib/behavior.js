const C = require("./constraints.js")
const S = require("./state.js")
const kast = require("./kast.js");

const genBehaviour = (state) => {
  const FILTER_OOG = state.config.FILTER_OOG || false;
  const behaviour = {
    id: state.initt,
    children: [],
    leaf: false,
    height: 0
  }
  var frontier = [
    behaviour
  ]
  var height = 0;
  const potential_leafes = [];

  while (frontier.length > 0) {
    let next_frontier = [];
    height ++;
    frontier.forEach((node, i) => {
      let steps = (state.edges[node.id] || []);
      if(steps.length == 0) {
        potential_leafes.push(node);
      }
      if(steps.length == 1) {
        node.id = steps[0].to;
        node.height++;
        next_frontier = [...next_frontier, node];
      }
      if(steps.length > 1) {
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
            leaf: false,
            children: [],
            height: node.height + 1,
            step,
            deltaC,
            deltaCRaw
          }
        });
        node.children = children;
        next_frontier = [...next_frontier, ...children]
      }
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
          node.oog = kast.get(k.term, "ethereum.evm.statusCode").label == "EVMC_OUT_OF_GAS_NETWORK"
          node.gas = kast.get(k.term, "ethereum.evm.callState.gas")
        }
      }
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

  const filter = f => node => {
    node.children = node.children
      .filter(f)
      .map(filter(f))
    return node.children.length == 1
      ? node.children[0]
      : node
  }

  propagate("success")(behaviour)
  propagate("oog")(behaviour)

  if(FILTER_OOG) filter(node => !node.oog)(behaviour)

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

  return behaviour;
}


module.exports = {
  genBehaviour
}
