const xs = require("xstream").default;
const dropRepeats = require("xstream/extra/dropRepeats").default;
const sampleCombine = require("xstream/extra/sampleCombine").default;
const xml2json = require("xml2json");
const _ = require("lodash");
const Constraints = require("./constraints.js");

const {show} = require("./show.js");
const toggleBtns = {};
Object.keys(show)
.forEach(key => {
  show[key].toggle.forEach(t => { toggleBtns[t] = key; })
})

module.exports = ({CLI, onion, K, Settings}) => {

  const initialReducer$ = xs.of(() => ({
    path: [],
    edges: {},
    nodes: {},
    normalnodes: {},
    circc: {},
    rules: {},
    crash: {},
    branch: {},
    halt: {},
    end: {},
    exception: {},
    revert: {},
    show: {
      term: true
    },
    steps: 0

  }))

  const stateUpdate$ = K
  .map(msg => {
    return (prev) => {
      let updated = {};
      if(msg.type === "sid") updated.sid = msg.data;
      if(msg.type === "initt") {
        updated.initt = msg.data[0]
        updated.path = [{
          type: "step",
          step: {
            from: "",
            to: msg.data[0],
            rule: "",
            step: 0
          }
        }];
      }
      if(msg.type === "targett") updated.targett = msg.data[0]
      if(msg.type === "error") {
        console.log(msg.data);
        updated.error = prev.error + "\n" + msg.data
      }
      if(msg.type === "step") {
        let step = msg.data[0];
        let from = msg.data[1];
        let to = msg.data[2];
        updated.edges = Object.assign({}, prev.edges);
        updated.edges[from] = (updated.edges[from] || [])
          .concat([{step, from, to}])
        updated.steps = Math.max(step, prev.steps);
        // TODO - build behaviour here
      }
      if(msg.type === "rstep") {
        let step = msg.data[0];
        let from = msg.data[1];
        let to = msg.data[2];
        let rule = msg.data[3];
        updated.edges = Object.assign({}, prev.edges);
        updated.edges[from] = (updated.edges[from] || [])
          .concat([{step, from, to, rule}])
        updated.steps = Math.max(step, prev.steps);
        // TODO - build behaviour here
      }
      if(msg.type === "node") {
        updated.nodes = Object.assign({}, prev.nodes);
        let clean_term_string = msg.data.node.term
          .replace(/\<\-/g,"--")
          .replace(/\&/g, "And")
          .replace(/s\<Word/g,"sLessWord")
          .replace(/\<Int/g,"lesInt")
          .replace(/\.\_\d\d\d\d:[^ ]+ /g, " ")
        let term = xml2json.toJson(clean_term_string, {object: true})
        let constraint = Constraints.clean(msg.data.node.constraint)
        updated.nodes[msg.data.id] = {
          term,
          constraint
        };
      }
      if(msg.type === "rule") {
        updated.rules = Object.assign({}, prev.rules);
        updated.rules[msg.data.id] = msg.data.rule;
      }
      if(msg.type === "normalnode") {
        updated.normalnodes = Object.assign({}, prev.normalnodes, {[msg.data[0]]: true});
      }
      if(msg.type === "specialnode") {
        updated[msg.data[0]] = Object.assign({}, prev[msg.data[0]], {[msg.data[1]]: true});
      }
      if(msg.type === "crash") {
        updated.crash = Object.assign({}, prev.crash, {[msg.data[0]]: true});
      }
      if(msg.type === "circcdata") {
        let circc = JSON.parse(msg.data.circc);
        updated.circc = Object.assign({}, prev.circc);
        let id = circc.term;
        if(!(id in updated.circc)) updated.circc[id] = [];
        let rhs = Constraints.clean(circc.rhs)//.replace("\\n", "\n").split("\n").map(s => s.trim()).join("\n");
        updated.circc[id] = _.uniq([rhs].concat(updated.circc[id]));
      }
      return Object.assign({}, prev, updated);
    }
  })

  const keyUpdate$ = CLI
  .map(key => prev => {
    let updated = Object.assign({}, prev);
    if(key in toggleBtns) updated.show[toggleBtns[key]] = !updated.show[toggleBtns[key]];

    if(/\d/.test(key)) {
      let id = prev.path[prev.path.length - 1].step.to;
      let steps = prev.edges[id];
      updated.path = prev.path.slice(0)
      while(steps && steps.length === 1) {
        updated.path.push({
          type: "step",
          step: steps[0]
        })
        steps = prev.edges[steps[0].to];
      }
      updated.path.push({
        type: "branch",
        branch: parseInt(key),
        step: steps[parseInt(key)]
      })
    }
    if(key == "N") {
      // todo test this for crashing in init
      let id = prev.path[prev.path.length - 1].step.to;
      let steps = prev.edges[id];
      if(steps && steps.length === 1) {
        updated.path = prev.path.concat({
          type: "step",
          step: steps[0]
        });
      }
    }
    if(key == "n") {
      let id = prev.path[prev.path.length - 1].step.to;
      let steps = prev.edges[id];
      updated.path = prev.path.slice(0)
      while(steps && steps.length === 1 && !(steps[0].to in prev.normalnodes)) {
        updated.path.push({
          type: "step",
          step: steps[0]
        })
        steps = prev.edges[steps[0].to];
      }
      if(steps && steps.length === 1 && (steps[0].to in prev.normalnodes)) {
        updated.path.push({
          type: "step",
          step: steps[0]
        })
      }
    }
    if(key == "\u000e") { // CTRL + n
      let id = prev.path[prev.path.length - 1].step.to;
      let steps = prev.edges[id];
      updated.path = prev.path.slice(0)
      while(steps && steps.length === 1) {
        updated.path.push({
          type: "step",
          step: steps[0]
        })
        steps = prev.edges[steps[0].to];
      }
    }
    if(key === "P" && updated.path.length > 1) {
      updated.path.pop();
    }
    if(key === "p") {
      if(updated.path.length > 1) {
        updated.path.pop();
      }
      while(updated.path.length > 1 && prev.edges[prev.path[prev.path.length - 1].step.to].length == 1 && !(prev.path[prev.path.length - 1].step.to in prev.normalnodes)) {
        updated.path.pop();
      }
    }
    if(key === "\u0010") { // CTRL + p
      if(updated.path.length > 1) updated.path.pop();
      while(updated.path.length > 1 && prev.edges[prev.path[prev.path.length - 1].step.to].length == 1) {
        updated.path.pop();
      }
    }
    return updated;
  })

  const reducer$ = xs.merge(
    initialReducer$,
    stateUpdate$,
    keyUpdate$
  )

  const initialRequest$ = Settings.map(({
      spec,
      lemmas,
      bin_runtime,
      replay,
      inspect
    }) => ({
      type: "run",
      data: {
        spec,
        lemmas,
        bin_runtime,
        replay,
        inspect
      }
    }));

  // const normalNodeMsgs$ = K
  //   .filter(msg => msg.type === "normalnode")
  //   .map(msg => msg.data[0])
  //
  // const normalNodeRequest$ = xs
  //   .combine(onion.state$, normalNodeMsgs$)
  //   .filter(([state, id]) => !(id in state.nodes))
  //   .map(([state, id]) => "getnode " + state.sid + " " + id)

  const toNodeRequest$ = onion.state$
    .filter(state => state && state.path && state.path.length > 0 && (s = state.path[state.path.length - 1]) && !(s.step.to in state.nodes))
    .map(state => state.sid + " " + state.path[state.path.length - 1].step.to)
    .compose(dropRepeats())
    .map(id => ({type:"getnode", data: id}))

  // const ruleRequest$ = onion.state$
  //   .filter(state => state && state.show.rule && state.path && state.path.length > 0 && (s = state.path[state.path.length - 1]) && !(s.step.rule in state.rules))
  //   .filter(state => state.path[state.path.length - 1].step.rule != "")
  //   .map(state => state.sid + " " + state.path[state.path.length - 1].step.rule)
  //   .compose(dropRepeats())
  const ruleRequest$ = K
    .filter(msg => msg.type == "newrule")
    .compose(sampleCombine(onion.state$))
    .map(([msg, state]) => ({type:"getrule", data: state.sid + " " + msg.data[0]}))

  const circcRequest$ = K
    .filter(msg => msg.type == "circc")
    .compose(sampleCombine(onion.state$))
    .map(([msg, state]) => ({type:"getcircc", data: state.sid + " " + msg.data[0]}))

  const branchingNodeRequest$ = K
    .filter(msg => msg.type == "specialnode" && msg.data[0] == "branch")
    .compose(sampleCombine(onion.state$))
    .map(([msg, state]) => ({type:"getnode", data: state.sid + " " + msg.data[1]}))

  const request$ = xs.merge(
    initialRequest$,
    toNodeRequest$,
    ruleRequest$,
    circcRequest$,
    branchingNodeRequest$
  );

  return {
    request$,
    reducer$
  };
}
