const clc = require('cli-color');
const modules = require("./modules.js");
const help = require("./help.js");
const S = require("./state.js");

const display = (name, {view, toggle}, state) => {
  let module = view(state);
  if(name != "source") module = module.split("\n").join("\n  ")
  return clc.bold(name) + "\n  " + module;
}

module.exports = ({onion}) => {

  let cli$ = onion.state$
  .map(state => {
    const tw = process.stdout.columns; // total width
    const cmd = !state.cmd_mode
      ? ""
      : clc.xterm(62)("cmd: ") + clc.bgXterm(237)(" " + (state.cmd || "") + "█" + " ".repeat(tw - (state.cmd || "").length - 7))

    const rhs = state.path.length > 0
      ? " " + state.path[state.path.length - 1].step.step + "/" + state.steps + " "
      : "?"
      ;
    const lhs = " KLab ";
    const mid = clc.bgXterm(235)(" ".repeat(tw - lhs.length - rhs.length))
    const status = clc.bgXterm(59)(lhs) + mid + clc.bgXterm(238)(rhs)

    if(!state || !state.path || state.path.length == 0) return {
      main: "loading...",
      cmd,
      status
    }
    let id = S.term_id(state)
    let steps = S.steps(state);
    if(!(id in state.nodes)) return null;

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

    const vs = Object.keys(state.show)
      .filter(key => state.show[key])
      .filter(key => key !== "branching")
      .map(key => display(key, modules[key], state))
      .join("\n\n")

    let main = vs + "" + branching
    return {
      main,
      cmd,
      status
    };
  })
  .filter(s => !!s)

  return cli$;
}
