const S = require('./state.js')
const format = require("./format.js")
const { genBehaviour, getPath } = require("./behavior.js");
const fs = require("fs");
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

    const global_behaviour = genBehaviour(state);
    const behaviour = getPath(global_behaviour, state.path)
    fs.writeFileSync('behaviour.json', JSON.stringify(behaviour, false, 2))
    const nextid = behaviour.children[parseInt(args[0])].first;
    updated.path = updated.path
      .slice(0, -1)
      .concat({
        type: "branch",
        branch: parseInt(args[0]),
        height: behaviour.children[parseInt(args[0])].height,
        step: state.coedges[nextid][parseInt(args[0])]
      }, {
        type: "step",
        count: 0,
        step: state.coedges[nextid][parseInt(args[0])]
      })
    return updated;
  },
  "next_k": state => {
    let updated = Object.assign({}, state);
    let id = S.id(state)
    let steps = state.edges[id];
    if(steps && steps.length === 1) {
      const old_head = state.path[state.path.length - 1]
      updated.path = state.path
      .slice(0, -1)
      .concat({
        type: "step",
        count: old_head.count + 1,
        step: steps[0]
      });
    }
    return updated;
  },
  "prev_k": state => {
    const updated = Object.assign({}, state);
    const old_head = state.path[state.path.length - 1]
    const steps = state.coedges[old_head.step.from]
    if(old_head.count > 0) {
      updated.path = updated.path
      .slice(0, -1)
      .concat({
        type: "step",
        count: old_head.count - 1,
        step: steps && steps[0] || {
          from: "",
          to: old_head.step.from,
          rule: ""
        }
      })
    } else if(updated.path.length > 1) {
      const last_branch = state.path[state.path.length - 2];
      const new_count   = state.path.length > 2
        && last_branch.height - state.path[state.path.length - 3].height
        || last_branch.height

      updated.path = updated.path
      .slice(0, -2)
      .concat({
        type: "step",
        count: new_count - 1,
        step: steps && steps[0] || {
          from: "",
          to: old_head.step.from,
          rule: ""
        }
      })

    }
    return updated;
  },
  "next_step": state => {
    let updated = Object.assign({}, state);
    let id = S.id(state);
    let steps = state.edges[id];
    var head = state.path[state.path.length - 1]
    // updated.path = state.path.slice(0)
    while(steps && steps.length === 1 && !(steps[0].rule in state.steps)) {
      head = {
        type: "step",
        step: steps[0],
        count: head.count + 1
      }
      steps = state.edges[steps[0].to];
    }
    if(steps && steps.length === 1 && (steps[0].rule in state.steps)) {
      head = {
        type: "step",
        step: steps[0],
        count: head.count + 1
      }
    }
    // TODO - manage hitting on a branching
    updated.path =
      updated.path
      .slice(0, -1)
      .concat(head);
    return updated;
  },
  "prev_step": state => {
    let updated = Object.assign({}, state);
    let id = state.path[state.path.length - 1].step.from
    let steps = state.coedges[id];
    var head = state.path[state.path.length - 1]
    var new_path = updated.path.slice(0, -1)
    while(steps && steps.length === 1 && !(steps[0].rule in state.steps)) {
      if(head.count == 0) {
        head.count = new_path.length > 1
          ? new_path[new_path.length - 1].height - new_path[new_path.length - 2].height
          : new_path[new_path.length - 1].height
        new_path = new_path.slice(0, -1)
      }
      head = {
        type: "step",
        step: steps[0],
        count: head.count - 1
      }
      steps = state.coedges[steps[0].from];
    }
    if(steps && steps.length === 1 && (steps[0].rule in state.steps)) {
      head = {
        type: "step",
        step: steps[0],
        count: head.count - 1
      }
    }
    // TODO - manage hitting on a branching
    updated.path = new_path
      .concat(head);
    return updated;
  },
  "next_branch": state => {
    let updated = Object.assign({}, state);
    let id = state.path[state.path.length - 1].step.to;
    var head = state.path[state.path.length - 1]
    let steps = state.edges[id];
    updated.path = state.path.slice(0)
    while(steps && steps.length === 1) {
      head = {
        type: "step",
        count: head.count + 1,
        step: steps[0]
      }
      steps = state.edges[steps[0].to];
    }
    updated.path = updated.path
    .slice(0, -1)
    .concat(head);
    return updated;
  },
  "prev_branch": state => {
    let updated = Object.assign({}, state);
    let id = state.path[state.path.length - 1].step.from;
    var head = state.path[state.path.length - 1]
    let steps = state.coedges[id];
    updated.path = state.path.slice(0)
    while(steps && steps.length === 1) {
      head = {
        type: "step",
        count: head.count - 1,
        step: steps[0]
      }
      steps = state.coedges[steps[0].from];
    }
    updated.path = updated.path
    .slice(0, -1)
    .concat(head);
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
  },
  "show" : (state, args) => {
    return {
      ...state,
      debug_show: [
        ...(state.debug_show || []),
        args[0]
      ]
    }
  },
  "hide" : (state, args) => {
    return {
      ...state,
      debug_show: (state.debug_show || []).filter(k => k !== args[0])
    }
  },
  "omit" : (state, args) => {
    return {
      ...state,
      term_omit: (state.term_omit || []).concat(args)
    }
  },
  "unomit" : (state, args) => {
    let current_omit = state.term_omit || []
    return {
      ...state,
      term_omit: current_omit.filter(k => k !== args[0])
    }
  },
  "scroll_down": (state, args) => {
    return {
      ...state,
      scroll: state.scroll + 1
    }
  },
  "scroll_up": (state, args) => {
    return {
      ...state,
      scroll: Math.max(state.scroll - 1, 0)
    }
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
