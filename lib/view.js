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

const makeStatus = (o) => {
  const tw = process.stdout.columns; // total width

  const lhs = clc.bgXterm(59)(" KLab ");
  const step_str = o.path.length > 0
    ? " " + o.path.length + "/" + o.steps + " "
    : ""
  ;

  var path = o.path
    .filter(s => s.type === "branch")
    .map(s => s.branch)
  if(o.is_branching) path = [...path, "? "]

  const format_path = "❯ " + path.join(" ❯ ")
  const connection_status =
    o.connection.status == "failed" && clc.bgXterm(234).xterm(203)(" • ")
    || o.connection.status == "error" && clc.bgXterm(234).xterm(160)(" • ")
    || o.connection.status == "closed" && clc.bgXterm(234).xterm(221)(" • ")
    || o.connection.status == "connected" && clc.bgXterm(234).xterm(83)(" • ")
    || ""

  const connection = o.connection.type == "remote"
    ? o.connection.host + " " + connection_status
    : "local"
  const app_status = ` ${o.status} `;
  const rhs = clc.bgXterm(238)(step_str)
    + clc.bgXterm(59)(app_status)
    + clc.bgXterm(242)(" " + connection)
  const mid_length = tw
    - clc.getStrippedLength(lhs)
    - clc.getStrippedLength(rhs)
    - clc.getStrippedLength(format_path)
    - 2
  const mid =
    clc.bgXterm(o.is_branching ? 88 : 235)(
      " " + format_path + " " +
      " ".repeat(mid_length)
    )
  return lhs + mid + rhs
}

module.exports = ({onion}) => {

  let cli$ = onion.state$
  .map(state => {
    const tw = process.stdout.columns; // total width
    const cmd = !state.cmd_mode
      ? ""
      : clc.xterm(62)("cmd: ") + clc.bgXterm(237)(" " + (state.cmd || "") + "█" + " ".repeat(tw - (state.cmd || "").length - 7))


    if(state.path.length == 0) return {
      main: "loading...",
      cmd,
      status: makeStatus({
        ...state,
        is_branching: false
      })
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
    let main = vs
    return {
      main,
      cmd,
      status: makeStatus({
        ...state,
        is_branching
      })
    };
  })
  .filter(s => !!s)

  return cli$;
}
