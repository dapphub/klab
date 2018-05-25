const clc = require('cli-color');

const genBehaviour = (state) => {
  let behaviour = "";
  let frontier = [{
    id: state.initt,
    path: "",
    leave: false
  }];
  let leaves = [];
  let terminate = false;
  let _visited = {
    [state.initt]: true
  };
  while (frontier.length > 0) {
    let next_frontier = [];
    frontier.forEach(e => {
      let next_steps = (state.edges[e.id] || []);

      if(next_steps.length == 0) {
        e.leave = true;
        if( e.id === state.targett ) e.path += clc.green(" ✓");
        if( e.id === state.crashHash ) e.path += " 💥";
        if( state.nodes[e.id] && state.nodes[e.id].term.k === "#exception" ) e.path += clc.red(" ✗e")
        if( state.nodes[e.id] && state.nodes[e.id].term.k === "#revert" ) e.path += clc.red(" ✗r")
        if( state.nodes[e.id] && state.nodes[e.id].term.k === "#end" ) e.path += clc.blue(" ⊥")
        leaves.push(e);
        return null;
      }

      let next_frontier_steps = next_steps
        .map((step, i) => ({
          id: step.to,
          path: e.path + (next_steps.length > 1 ? i + "." : ""),
          leave: false
        }))
      next_frontier = next_frontier.concat(next_frontier_steps);
    });
    frontier = next_frontier;
  }
  behaviour = leaves
  .map(branch => branch.path)
  .sort()
  .join("\n")
  return behaviour;
}
module.exports = genBehaviour;
