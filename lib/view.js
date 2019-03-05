const clc = require('cli-color');
const modules = require("./modules.js");
const S = require("./state.js");
const Constraint = require("./constraints.js")

const display = (name, {view, toggle}, state) => {
  let module = view(state);
  if(name != "source") module = module
  return clc.bold(name) + "\n" + module;
}

const makeStatus = (o) => {
  const tw = process.stdout.columns; // total width

  const lhs = clc.bgXterm(59)(" KLab ");
  const getHeights = b => [b.endheight].concat(b.children.map(getHeights).reduce((a,c) => a.concat(c), []))

  const max_step = o.behaviour &&
    Math.max.apply(null, o.behaviour && getHeights(o.behaviour) || [0])

  const step_str = o.path.length > 0
    ? " " + ((o.path.length > 1 ? o.path.slice(-2, -1)[0].height : 0 ) + o.path.slice(-1)[0].count)  + "/" + max_step + " "
    : ""
  ;

  // const format_path = (o.config.proofid || "").slice(0, 8) + "… ❯ " + path.join(" ❯ ")
  const connection_status =
    o.connection && (
      o.connection.status == "failed" && 88
    || o.connection.status == "error" && 88
    || o.connection.status == "closed" && 130
    || o.connection.status == "connected" && 29
    ) || ""

  const app_status = ` ${o.status} `;
  const rhs = clc.bgXterm(238)(step_str)
    + clc.bgXterm(o.status == "pass" ? 29 : 240)(app_status)
  const mid_length = tw
    - clc.getStrippedLength(lhs)
    - clc.getStrippedLength(rhs)
    // - clc.getStrippedLength(format_path)
    - 2
  const mid =
    clc.bgXterm(o.is_branching ? 88 : 235)(
      " "
      // + format_path
      + " "
      + " ".repeat(mid_length)
    )
  return lhs + mid + rhs
}

module.exports = ({onion}) => {

  let cli$ = onion.state$
  .map(state => {
    const tw = process.stdout.columns; // total width
    const cmd = !state.cmd_mode
      ? ""
      : ":" + (state.cmd || "") + "█" + " ".repeat(tw - (state.cmd || "").length - 7)

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
    const vs = Object.keys(state.show || {})
      .filter(key => state.show[key] || (false && is_branching && key == "behaviour"))
      .map(key => display(key, modules[key], state))
      .join("\n\n")


    // STATUS
    let main = vs.split("\n").slice(state.scroll).join("\n")
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
