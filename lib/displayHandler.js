const xs = require("xstream").default;
const _ = require("lodash");
const clc = require('cli-color');
const dropRepeats = require("xstream/extra/dropRepeats").default;
const sampleCombine = require("xstream/extra/sampleCombine").default;
const jsondiffpatch = require("jsondiffpatch").create({
   textDiff: {
        // default 60, minimum string length (left and right sides) to use text diff algorythm: google-diff-match-patch
        minLength: 10
    }
});
const {show, formatRule} = require("./show.js");
const displayHelp = require("./displayHelp.js");

const display = (name, {exec, toggle}, state, settings) => {
  let module = exec(state, settings);
  if(name != "source") module = module.split("\n").join("\n  ")
  return clc.bold(name) + "\n  " + module;
}

module.exports = ({onion, Settings}) => {

  let cto$ = onion.state$
    .map(state =>
      state &&
      state.path &&
      state.path.length > 0 &&
      state.path[state.path.length - 1] &&
      (to = state.path[state.path.length - 1].step.to) &&
      state.nodes[to]
    )
    .filter(e => !!e)
    .compose(dropRepeats())

  let cshow$ = onion.state$
    .map(state => JSON.stringify(state.show))
    .compose(dropRepeats())

  let renderTrigger$ = xs.merge(cshow$, cto$);

  let cli$ = renderTrigger$.compose(sampleCombine(xs.combine(onion.state$, Settings)))
  .map(([a, b]) => b)
  .map(([state, settings]) => {
    if(!state || !state.path || state.path.length == 0) return "loading...";
    let id = state.path[state.path.length - 1].step.to;
    let steps = state.edges[id];
    if(!(id in state.nodes)) return "loading ...";
    let branching = "";
    if(steps && steps.length > 1) {
      branching = clc.bold("branching:\n") + steps
        .map((step, i) => "  " + i + ". " + formatRule(state.rules[step.rule]))
        .join("\n\n")
    }
    let circc = "";
    if(steps && steps[steps.length - 1].from in state.circc) {
      circc = "circc:\n"+state.circc[steps[steps.length - 1].from].join("\n")
    }

    return displayHelp(state) + Object.keys(state.show)
      .filter(key => state.show[key])
      .filter(key => key !== "branching")
      .map(key => display(key, show[key], state, settings))
      .join("\n\n")+"\n\n"+branching+"\n\n"+circc
  })

  return {
    cli$
  }
}
