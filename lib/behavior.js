const C = require("./constraints.js")
const S = require("./state.js")
const clc = require('cli-color');

const genBehaviour = (state) => {
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
        let children = steps.map((step, i) => ({
          index: i,
          head: i + (node.status || ""),
          id: step.to,
          leaf: false,
          children: [],
          height: node.height + 1,
          step,
          deltaC: C.deltaC(state, step)
        }));
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
        node.status = clc.green("✓");
        node.success = true;
      }
      if(node.id in (state.revert || {})) {
        node.status = clc.orange("↩");
      }
      if(node.head) node.head = (node.head || node.index) + (node.status ? " " + node.status : "");
    })

  let propagate_success = node => {
    if(node.leaf) return node.success;
    let success = node.children
      .map(propagate_success)
      .reduce((a,b) => a && b, true)
    node.success = success;
    return success;
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

  propagate_success(behaviour)

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

  // while (frontier.length > 0) {
  //   let next_frontier = [];
  //   frontier.forEach(e => {
  //     let next_steps = (state.edges[e.id] || []);
  //
  //     if(next_steps.length == 0) {
  //       e.leaf = true;
  //       if( e.id === state.targett ) e.path += clc.green(" ✓");
  //       if( e.id in state.crash) e.path += " 💥";
  //       if( e.id in state.exception ) e.path += clc.red(" ✗e")
  //       if( e.id in state.revert ) e.path += clc.red(" ✗r")
  //       if( e.id in state.halt ) e.path += clc.blue(" ⊥")
  //       if( e.id in state.end ) e.path += clc.blue(" ⊥")
  //       leafs.push(e);
  //       return null;
  //     }
  //
  //     let next_frontier_steps = next_steps
  //       .map((step, i) => ({
  //         id: step.to,
  //         path: e.path + (next_steps.length > 1 ? i + "." : ""),
  //         leaf: false
  //       }))
  //     next_frontier = next_frontier.concat(next_frontier_steps);
  //   });
  //   frontier = next_frontier;
  // }
  return behaviour;
  // .map(branch => branch.path)
  // .sort()
  // .join("\n")
}


module.exports = {
  genBehaviour
}
