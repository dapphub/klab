var draw = ({ path, show, nodes }) => {
  let step = path[path.length - 1].step;
  let node_id = step.to;
  let node = getNode(node_id);
  let isSuccess = node_id == target_id;
  let isCrash   = node_id == crashHash;

  let term_ = _.omit(node.term, settings.omit).generatedTop;
  term_ = JSON.parse(JSON.stringify(term_));
  if(term_.ethereum.evm.txExecState.wordStack.length > tw / 2) {
    term_.ethereum.evm.txExecState.wordStack = term_.ethereum.evm.txExecState.wordStack.split(": ").join("\n");
  }
  let node_term = term_;

// help
  let help_strings = help(show, 2);
  let help_display = help_strings.map(help_string => {
    let prefix = (tw - clc.getStrippedLength(help_string)) / 2
    console.log(" ".repeat(prefix) + help_string );
  })
  console.log("-".repeat(tw));

// behaviour path
  let step_counter = step.step; //path.length;
  let format_path = "root.";
  path.forEach(s => {
    if(s.type === "branch") format_path += s.branch + ".";
  });
  let format_step = "step: " + step_counter;
  let format_feedback = (isCrash ? " 💥" : "") + (isSuccess ? clc.green(" ✓") : "")
  let lhs = 'Behaviour: ' + format_path + format_feedback;
  let rhs = format_step;
  let nav = lhs + " ".repeat(tw - (lhs.length + rhs.length)) + rhs;
  console.log(nav + "\n");

// term
  let term;
  if(show.trim) {
    const trim = o => {
      if( typeof o == "object" ) {
        let o_ = {};
        Object.keys(o).forEach(k => {
          o_[k] = trim(o[k])
        })
        return o_;
      }
      else if(typeof o == "string" ) {
        return o.slice(0, 110)
      }
      return o;
    }
    node_term = trim(node_term);
  }
  if(path.length > 1) {
    term = formatStep(node_term, getNode(step.from).term);
  } else {
    term = formatStep(node_term, {});
  }
  console.log(term);

// source code
  if(show.source) console.log(`\n${clc.bold(clc.underline("s")+"ource")}:\n` + getCodeStringFromPc(node_term.ethereum.evm.txExecState.pc));

// memory
  if(show.memory) {
    let memory_string = node.term.generatedTop.ethereum.evm.txExecState.localMem;
    let formatted_memory = formatMemory(memory_string);
    console.log(`${clc.bold(clc.underline("m") + "emory")}:` + formatted_memory);
  }

// debug
  if(show.debug) console.log(`\ndebug:\nnode_id: ${node_id}\npath length: ${path.length}`);

// constraint
  let constraint;
  if(path.length > 1) {
    if(node.constraint !== getNode(step.from).constraint) {
      constraint = clc.yellow(node.constraint);
    } else {
      constraint = node.constraint;
    }
  } else {
    constraint = node.constraint;
  }
  constraint = constraint
    .split("\n")
    .join("")
    .split("#And")
    .map(s => "  " + s.replace(/ ==K true/g,"").trim())
    .join("\n")
    .replace(/115792089237316195423570985008687907853269984665640564039457584007913129639936/g,"pow256")
    .replace(/1461501637330902918203684832716283019655932542976/g,"pow160")
    .replace(/Int/g,"")
  if(show.constraint) console.log(`\n${clc.bold(clc.underline("c")+"onstraint")}:\n  `+constraint.split("\n").join("\n  "));

// Rule
  if(show.rule) {
    let rule = getRule(step.rule) || "";
    if(rule && rule.string) console.log(`\n${clc.bold(clc.underline("r") + "ule")}:\n  ` + rule.string);
  }

// global Behaviour
  if(show.behaviour) {
    let filtered = behaviour
      .split("\n")
      .filter(b => b.indexOf(format_path.slice(5)) == 0)
      .map(b => clc.green(format_path.slice(5)) + b.slice(format_path.length - 5))
      .join("\n  ")
    console.log(`\n${clc.bold(clc.underline("b") + "ehaviour")}:\n  ` + filtered);
  }


// Branching
  let next_steps = E[node_id];
  if(next_steps && next_steps.length > 1) {
    let pc = node_term.ethereum.evm.txExecState.pc;
    let printSemantics = key => key in settings.semantic && clc.green(settings.semantic[key].text) + "\n    " || "";
    let branching_string = next_steps.map(({rule}, i) => i + ". "+printSemantics(pc +":"+ getRule(rule).from)+" " + getRule(rule).string).join("\n  ");
    console.log(`\n${clc.bold("branching")}: ${next_steps.length}\n  ${branching_string}`);
  }

// terminal
  if(isSuccess) {
    console.log(clc.green(` ✓ Target Term Reached!`));
  }

// crash
  if(isCrash) {
    let ruleString = parseRule(crashRule).string;
    console.log(clc.red("\nCrash occurred during rule:\n") + ruleString);
    console.log("\n" + crashMsg.split("\n").slice(1).join("\n"));
  }
}
