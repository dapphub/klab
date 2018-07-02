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
    let steps = S.steps(state);
    // TODO - do I need this here?
    if(!(S.term_id(state) in state.nodes)) return null;
    const is_branching = steps && steps.length > 1;


    // MAIN
    const vs = Object.keys(state.show)
      .filter(key => state.show[key] || (is_branching && key == "behaviour"))
      .map(key => display(key, modules[key], state))
      .join("\n\n")


    // STATUS
    const step_str = state.path.length > 0
      ? " " + state.path.length + "/" + state.steps + " "
      : ""
      ;
    var path = state.path
      .filter(s => s.type === "branch")
      .map(s => s.branch)
    if(is_branching) path = [...path, "? "]
    const format_path = "❯ " + path.join(" ❯ ")
    const app_status = ` ${state.status} `;
    const rhs = clc.bgXterm(238)(step_str) + clc.bgXterm(59)(app_status);
    const mid_length = tw
      - clc.getStrippedLength(lhs)
      - clc.getStrippedLength(rhs)
      - clc.getStrippedLength(format_path)
      - 3
    const mid =
      clc.bgXterm(is_branching ? 88 : 235)(
        " " + format_path + " " +
        " ".repeat(mid_length)
      )
    const status = lhs + mid + rhs

    let main = vs
    return {
      main,
      cmd,
      status
    };
  })
  .filter(s => !!s)

  return cli$;
}
