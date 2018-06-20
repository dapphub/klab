const _ = require("lodash");
const clc = require('cli-color');
// const { formatStep } = require("./formatter.js");
const { formatMemory } = require("./unwrapper.js");
const { getCodeStringFromPc } = require("../lib/srchandler.js");
const genBehaviour = require("./helper.js");
const Constraints = require("./constraints.js");


const formatRule = rule => {
  if(!rule) return "?"
  let string = clc.xterm(0)(`${rule.filepath} ${rule.from}-${rule.to}\n     `) + rule.string.split("\n").join("\n  ").trim();
  if(string.split("\n").length > 6) {
    string = string.split("\n").slice(0, 3)
    .concat([clc.red("  [...]")])
    .concat(string.split("\n").slice(-3))
    .join("\n")
  }
  return string;
}

const getKJSONPath = (termList, pathComponents) => {
  if (pathComponents.length == 0) return termList
  let pathComponent = pathComponents[0]
  let subTerm = termList.find( term => term.node == "KApply" && term.label == pathComponent )
  return getKJSONPath(subTerm.args, pathComponents.slice(1))
}

const getSpacing = (dataLists) => {
  const minWidth = (acc, cur, i, a) => {
    let curWidths = cur.map(str => str.length)
    // TODO: Do not know how to zip things in JS
    return [...Array(acc.length).keys()].map(i => Math.max(curWidths[i], acc[i]))
  }
  return dataLists.reduce(minWidth, dataLists[0].map(l => 0))
}

const padWidths = (dataList, targetWidths) => {
  return [...Array(dataList.length).keys()].map(i => dataList[i].padEnd(targetWidths[i]))
}

const printTermLine = (tLine, tWidths) => {
  return padWidths(tLine, tWidths).join("    ").split("\n").join(" ").slice(0, process.stdout.columns - 4)
}

const showConstraint = (constraint) => {
  if (constraint.label == "#And") {
    return constraint.args.map( arg => "    " + arg.token ).join("\n")
  } else if (constraint.arity == 0) {
    return constraint.label
  } else {
    return constraint.token
  }
}

const view = {
  term: {
    view: (state, settings) => {
      let id = state.path[state.path.length - 1].step.to.split("_")[0]
      let cells = getKJSONPath([state.nodes[id].term], ["<generatedTop>"])
      let cellPathList = [ [ "<k>" ]
                         , [ "<ethereum>" , "<evm>" , "<callState>" , "<wordStack>" ]
                         , [ "<ethereum>" , "<evm>" , "<callState>" , "<localMem>"  ]
                         ]
      let termData = cellPathList.map( cellPath => [cellPath[cellPath.length - 1], getKJSONPath(cells, cellPath)[0].token] )
      let termWidths = getSpacing(termData)
      // return JSON.stringify(cells, false, 2);
      return termData.map(td => printTermLine(td, termWidths)).join("\n")
    },
    toggle: ["t", "T"]
  },
  constraint: {
    view: (state, settings) => {
      let id = state.path[state.path.length - 1].step.to.split("_")[1]
      return showConstraint(state.nodes[id].term)
    },
    toggle: ["c", "C"]
  },
  source: {
    view: (state, settings) => {
      let id = state.path[state.path.length - 1].step.to;
      let term = state.nodes[id].term;
      return getCodeStringFromPc(settings, term.generatedTop.ethereum.evm.callState.pc)
    },
    toggle: ["s", "S"]
  },
  rule: {
    view: (state, settings) => {
      let id = state.path[state.path.length - 1].step.rule;
      let rule = state && state.rules && state.rules[id] && formatRule(state.rules[id]) || "";
      return rule;
    },
    toggle: ["r", "R"]
  },
  behaviour: {
    view: (state, settings) => {
      let behaviour = genBehaviour(state);
      let format_path = "root.";
      state.path.forEach(s => {
        if(s.type === "branch") format_path += s.branch + ".";
      });
      let filtered = behaviour
        .split("\n")
        .filter(b => b.indexOf(format_path.slice(5)) == 0)
        .map(b => clc.green(format_path.slice(5)) + b.slice(format_path.length - 5))
        .join("\n")
      return filtered;
    },
    toggle: ["b", "B"]
  },
  debug: {
    view: (state, settings) => {
      let debug = {
        sid: state.sid
      };
      return JSON.stringify(debug, false, 2);
    },
    toggle: ["d"]
  },
  memory: {
    view: (state, settings) => {
      let id = state.path[state.path.length - 1].step.to;
      let term = state.nodes[id].term;
      let memory_string = term.generatedTop.ethereum.evm.callState.localMem;
      return memory_string.split("\n").map(s => s.trim()).join("\n");
      // let formatted_memory = formatMemory(memory_string);
      // return formatted_memory;
    },
    toggle: ["m", "M"]
  },
  z3feedback: {
    view: (state, settings) => {
      let nodeId = state.path[state.path.length - 1].step.to
      if (! (nodeId in state.z3feedback)) return ""
      let z3feedback = state.z3feedback[nodeId]
      let rhs        = showConstraint(z3feedback.rhs.term)
      let z3QueryId  = z3feedback.queryId
      let z3Result   = z3feedback.result.term.token
      return "result: " + clc.xterm(210)(z3Result)   + "\n"
           + "queryId: " + z3QueryId                 + "\n"
           + "checking:"                             + "\n"
           + "    " + rhs.split("\n").join("\n    ") + "\n"
    },
    toggle: ["z", "Z"]
  }
}

module.exports = view
