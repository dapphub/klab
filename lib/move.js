const modules = require("./modules.js");
const toggleBtns = {};
Object.keys(modules)
.forEach(key => {
  modules[key].toggle.forEach(t => { toggleBtns[t] = key; })
})

// TODO - meme
// TODO - figure out how to work directly on state
//        and not on a weird updated object
const controlObject = {
  "toggle_module": (state, key) => {
    let updated = Object.assign({}, state);
    if(key in toggleBtns) updated.show[toggleBtns[key]] = !updated.show[toggleBtns[key]];
    return updated;
  },
  "branch": (state, key) => {
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
    if(typeof key == "string" && typeof parseInt(key) == "number" && steps && steps[parseInt(key)]) {
      updated.path.push({
        type: "branch",
        branch: parseInt(key),
        step: steps[parseInt(key)]
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
    while(updated.path.length > 1 && state.edges[state.path[state.path.length - 1].step.to].length == 1 && !(state.path[state.path.length - 1].step.to in state.normalnodes)) {
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
    if(cmd in controlObject && state.path.length >= 1) {
      return controlObject[cmd](state, key);
    } else {
      return state;
    }
  })

module.exports = controlUpdate
