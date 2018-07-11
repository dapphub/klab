const modules = require("./modules.js");
// const toggleBtns = {};
// Object.keys(modules)
// .forEach(key => {
//   modules[key].toggle.forEach(t => { toggleBtns[t] = key; })
// })

// TODO - meme
// TODO - figure out how to work directly on state
//        and not on a weird updated object
const controlObject = {
  "toggle_module": (state, args) => {
    return {
      ...state,
      show: {
        ...state.show,
        [args[0]]: !state.show[args[0]]
      }
    };
  },
  "branch": (state, args) => {
    let updated = Object.assign({}, state);
    let id = state.path[state.path.length - 1].step.to;
    let steps = state.edges[id];
    updated.path = state.path.slice(0)
    while(steps && steps.length === 1) {
      updated.path.push({
        type: "step",
        step: steps[0]
      })
      steps = state.edges[steps[0].to];
    }
    if(typeof args[0] == "string" && typeof parseInt(args[0]) == "number" && steps && steps[parseInt(args[0])]) {
      updated.path.push({
        type: "branch",
        branch: parseInt(args[0]),
        step: steps[parseInt(args[0])]
      })
      // steps = state.edges[steps[parseInt(key)].to];
    }
    return updated;
  },
  "next_k": state => {
    let updated = Object.assign({}, state);
    let id = state.path[state.path.length - 1].step.to;
    let steps = state.edges[id];
    if(steps && steps.length === 1) {
      updated.path = state.path.concat({
        type: "step",
        step: steps[0]
      });
    }
    return updated;
  },
  "prev_k": state => {
    let updated = Object.assign({}, state);
    if(updated.path.length > 1) updated.path.pop();
    return updated;
  },
  "next_step": state => {
    let updated = Object.assign({}, state);
    let id = state.path[state.path.length - 1].step.to;
    let steps = state.edges[id];
    updated.path = state.path.slice(0)
    while(steps && steps.length === 1 && !(steps[0].rule in state.steps)) {
      updated.path.push({
        type: "step",
        step: steps[0]
      })
      steps = state.edges[steps[0].to];
    }
    if(steps && steps.length === 1 && (steps[0].rule in state.steps)) {
      updated.path.push({
        type: "step",
        step: steps[0]
      })
    }
    return updated;
  },
  "prev_step": state => {
    let updated = Object.assign({}, state);
    if(updated.path.length > 1) {
      updated.path.pop();
    }
    while(updated.path.length > 1 && state.edges[state.path[state.path.length - 1].step.to].length == 1 && !(state.path[state.path.length - 1].step.rule in state.steps)) {
      updated.path.pop();
    }
    return updated;
  },
  "next_branch": state => {
    let updated = Object.assign({}, state);
    let id = state.path[state.path.length - 1].step.to;
    let steps = state.edges[id];
    updated.path = state.path.slice(0)
    while(steps && steps.length === 1) {
      updated.path.push({
        type: "step",
        step: steps[0]
      })
      steps = state.edges[steps[0].to];
    }
    return updated;
  },
  "prev_branch": state => {
    let updated = Object.assign({}, state);
    if(updated.path.length > 1) updated.path.pop();
    while(updated.path.length > 1 && state.edges[state.path[state.path.length - 1].step.to].length == 1) {
      updated.path.pop();
    }
    return updated;
  },
  "skip_from": state => {
    let updated = Object.assign({}, state);
    updated.skipFrom = S.id(updated);
    return updated;
  },
  "skip_to" : state => {
    let updated = Object.assign({}, state);
    if (updated.skipFrom) {
      updated.shortcuts = (updated.shortcuts || []).concat([updated.skipFrom, S.id(updated)])
      updated.skipFrom = false;
    }
    return updated;
  }
}

// TODO - type data format
const controlUpdate = cmd$ => cmd$
  .map(({cmd, key}) => state => {
    let cmd_key = cmd.split(" ")[0];
    let cmd_args = cmd.split(" ").slice(1);
    if(cmd_key in controlObject && state.path.length >= 1) {
      return controlObject[cmd_key](state, cmd_args);
    } else {
      return state;
    }
  })

module.exports = controlUpdate
