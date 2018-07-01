const clc = require('cli-color');
const modules = require("./modules.js");
const help = require("./help.js");
const S = require("./state.js");
const Constraint = require("./constraints.js")

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

    const lhs = clc.bgXterm(59)(" KLab ");

    if(state.path.length == 0) return {
      main: "loading...",
      cmd,
      status: lhs
    }
    let id = S.term_id(state)
    let steps = S.steps(state);
    if(!(id in state.nodes)) return null;

    let branching = "";
    const is_branching = steps && steps.length > 1;
    if(is_branching) {
      let deltaC = step => {
        if(step.from.split("_")[1] in state.nodes && step.to.split("_")[1] in state.nodes) {
          let c1 = Constraint
            .clean(state.nodes[step.from.split("_")[1]].term.args.map(c => c.token))
            .split("\n")
          let c2 = Constraint
            .clean(state.nodes[step.to.split("_")[1]].term.args.map(c => c.token))
            .split("\n")
          return c2
            .filter(c => c1.indexOf(c) == -1)
            .map(c => clc.xterm(215)(c))
            .join("\n     ") + "\n     ";
        } else {
          return ""
        }
      }
      branching = "\n" + clc.bold("branching:\n") + steps
        .map((step, i) => "  " + i + ". " + deltaC(step)) // + formatRule(state.rules[step.rule])
        .join("\n")
    }

    const vs = Object.keys(state.show)
      .filter(key => state.show[key] || (is_branching && key == "behaviour"))
      .map(key => display(key, modules[key], state))
      .join("\n\n")



    // STATUS
    const step_str = state.path.length > 0
      ? " " + state.path.length + "/" + state.steps + " "
      : ""
      ;
    const app_status = ` ${state.status} `;
    const rhs = clc.bgXterm(238)(step_str) + clc.bgXterm(59)(app_status);
    const mid = clc.bgXterm(is_branching ? 137 : 235)(" ".repeat(tw - clc.getStrippedLength(lhs) - clc.getStrippedLength(rhs)))
    const status = lhs + mid + rhs

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
