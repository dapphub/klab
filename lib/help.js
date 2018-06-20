const clc = require('cli-color');
const modules = require("./modules.js");

const help = state => {
  let format_toggle = is => is ? clc.green("+") : clc.red("-");
  let module_activity = Object.keys(modules)
    .map(key => `${clc.underline(key[0])}${key.slice(1)}(${format_toggle(state.show[key])})`)
  let h_ = [
    "0-5/n next choice",
    "N next branching point",
    "Ctrl-c quit",
    "p previous",
  ].concat(module_activity)

  let h_a = h_.slice(0, Math.floor(h_.length / 2));
  let h_b = h_.slice(Math.floor(h_.length / 2));
  let h_as = h_a.join("     ");
  let h_bs = h_b.join("     ");
  let tw = process.stdout.columns; // total width
  let help_display = [h_as, h_bs]
    .map(help_string => {
    let prefix = (tw - clc.getStrippedLength(help_string)) / 2
    return " ".repeat(Math.max(prefix, 0)) + help_string
  }).join("\n") + "\n" + "-".repeat(tw || 0);

  let step = state.path[state.path.length - 1].step;
  let step_counter = step.step; //path.length;
  let format_path = "root.";
  state.path.forEach(s => {
    if(s.type === "branch") format_path += s.branch + ".";
  });
  let format_step = "step: " + step_counter + "/" + state.steps;
  // let format_feedback = (isCrash ? " 💥" : "") + (isSuccess ? clc.green(" ✓") : "")
  let lhs = 'Behaviour: ' + format_path// + format_feedback;
  let rhs = format_step;
  let nav = lhs + " ".repeat(tw - (lhs.length + rhs.length)) + rhs;

  return nav;
}

module.exports = help;
