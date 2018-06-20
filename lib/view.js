const xs = require("xstream").default;
const _ = require("lodash");
const clc = require('cli-color');
const dropRepeats = require("xstream/extra/dropRepeats").default;
const sampleCombine = require("xstream/extra/sampleCombine").default;
const modules = require("./modules.js");
const help = require("./help.js");

const display = (name, {view, toggle}, state) => {
  let module = view(state);
  if(name != "source") module = module.split("\n").join("\n  ")
  return clc.bold(name) + "\n  " + module;
}

module.exports = ({onion}) => {

  let cli$ = onion.state$
  .map(state => {
    if(!state || !state.path || state.path.length == 0) return "loading...";
    let id = state.path[state.path.length - 1].step.to.split("_")[0];
    let steps = state.edges[id];
    if(!(id in state.nodes)) return "loading ...";

    let branching = "";
    if(steps && steps.length > 1) {
      let deltaC = step => {
        if(step.from in state.nodes && step.to in state.nodes) {
          let c1 = state.nodes[step.from].constraint.split("\n").map(c => c.trim());
          let c2 = state.nodes[step.to].constraint.split("\n").map(c => c.trim())
          return c2
            .filter(c => c1.indexOf(c) == -1)
            .map(c => clc.xterm(215)(c))
            .join("\n     ") + "\n     ";
        } else {
          return ""
        }
      }
      branching = clc.bold("branching:\n") + steps
        .map((step, i) => "  " + i + ". " + deltaC(step)+ formatRule(state.rules[step.rule]))
        .join("\n\n")
    }
    let circc = "";
    if(steps && steps[steps.length - 1].from in state.circc) {
      circc = "circc:\n"+state.circc[steps[steps.length - 1].from].join("\n\n")
    }

    const tw = process.stdout.columns; // total width
    const cmd = clc.bgXterm(237)((state.cmd || "") + " ".repeat(tw - (state.cmd || "").length))
    const h = help(state);
    const vs = Object.keys(state.show)
      .filter(key => state.show[key])
      .filter(key => key !== "branching")
      .map(key => display(key, modules[key], state))
      .join("\n\n")

    return h + vs + "\n\n" + branching + "\n\n" + circc + "\n" + (state.cmd_mode ? cmd : "")
  })

  return cli$;
}
