const clc = require('cli-color');
const _ = require('lodash')
// const { formatStep } = require("./formatter.js");
const { formatMemory } = require("./unwrapper.js");
const C = require("./constraints.js");
const S = require("./state.js");
const Rule = require("./rule.js");
const kast = require("./kast.js");
// const Constraint = require("./constraints.js")
const format = require("./format.js")
const { genBehaviour } = require("./behavior.js");
const evmv = require("./evmv.js");
const getGasExpr = require('./gas.js');
const fs = require("fs");
const {
  getCodeStringFromPc,
} = require("../lib/srchandler.js");
const {
  buildDisplayInfo
} = require("../lib/clean-evm-node.js");

const hide_color = 244;
const show_color = 255;


const view = {
  constraint: {
    view: (state) => {
      let c1 = S
        .const(state)
        .split("\n")
        .map(c => c.trim())
      let initt_c = state.initt.split("_")[1];

      let cs = state.nodes[initt_c]
        .args
        .map(kast.format);
      let c2 = C.clean(cs)
        // .const(state.initt)
        // .split("\n")
        // .map(c => c.trim())
      let str = c1
        .map(c => c2.indexOf(c) == -1
          ? c
          : clc.xterm(244)(c)
        )
        .join("\n")
      return str;

    },
  },
  // TODO - evm specific
  evm: {
    view: evmv
  },
  gas : {
    view: state => {
      return getGasExpr(state);
    }
  },
  sourcemap: {
    view: state => {
      // SOURCECODE
      let id = S.term_id(state)
      let k = state.nodes[id];
      const {
        pc,
        call_id
      } = buildDisplayInfo(k);
      let contract_o = state.config.implementations[state.config.v2n[call_id]];
      let contract = state.config.contracts[contract_o.name];
      let src = getCodeStringFromPc(state.config.srcs, contract, parseInt(pc), true);
      return src;
    }
  },
  rule: {
    view: (state) => {
      let id = S.rule(state);
      let rule = state.rules[id] && Rule.formatRule(Rule.parseRule(state.rules[id].att)) || '';
      return rule;
    },
  },
  behaviour: {
    view: (state) => {
      const styles = node => !node && 22
        || node.branching && 215
        || node.in_history && show_color // 77
        || node.active && 244
        || 234

      const { behaviour } = genBehaviour(state);
      const table = format.foldPreorder({
        tree: JSON.parse(JSON.stringify(behaviour)),
        loc: "head",
        styles
      })
      return "  " + format
        .formatDb(table, ["head", "deltaC"], styles)
        .split("\n")
        .join("\n  ")
      + "\n"
      + state.path.map(step => `${step.type}(${JSON.stringify(_.omit(step, ["step", "type"])).replace(/\"|\{|\}/g, '')})`).join(".")
    },
  },
  debug: {
    view: (state) => {
      let id = S.term_id(state)
      let rid = S.id(state)
      let edges = state.edges[rid];
      let k = state.nodes[id];
      let debug = (state.debug_show || [])
        .reduce((a, key) => ({...a, [key]: kast.get(k, key)}), {})
      return "pid:    " + state.config.proofid
        + "\nid:     " + id
        + "\ntarget: " + state.targett
        + "\noutput: " + JSON.stringify(kast.get(k, "ethereum.evm.output"))
        + '\nedges: ' + JSON.stringify(edges, false, 2)
        + "\n" + JSON.stringify(debug, false, 2);

    },
  },
  memory: {
    view: (state) => {
      let id = S.term_id(state)
      let k = state.nodes[id];
      let mem = kast.get(k, "ethereum.evm.callState.localMem")
      return JSON.stringify(mem.args.map(l => kast.format(l.args[0]) + " " + kast.format(l.args[1])), false, 2)
      // return memory_string.split("\n").map(s => s.trim()).join("\n");
      // let formatted_memory = formatMemory(memory_string);
      // return formatted_memory;
    },
  },
  z3feedback: {
    view: (state) => {
      let nodeid = S.id(state);
      // return JSON.stringify(state.z3feedback[nodeid] || {}, false, 2)
      if (! (nodeid in state.z3feedback)) return "";
      let rs = state.z3feedback[nodeid]
        .filter(msg => msg.rule !== "NORULE")
        .reduce((a, r) => ({
          ...a,
          [r.rule]: [...(a[r.rule] || []), r]
        }), {})

      let str = Object.keys(rs)
        .map(ruleid => {
          let z3s = rs[ruleid];
          if(ruleid in state.rules) {
            // todo parsing is server
            // let rO = Rule.parseRule(state.nodes[ruleid].term.att);
            // return ruleid
            //   + " "
            //   + (ruleid in state.rules)
            //   + " "
            //   + JSON.stringify(state.rules[ruleid], false, 2)

            return Rule.formatRule(state.rules[ruleid])
              + "\n"
              + z3s.map(({query, implication, result}) => {
                let resultStr = result in state.nodes
                  && state.nodes[result].token
                  || "";
                let imp_str = implication.split("_")[1] in state.nodes
                  && C.clean(kast.format( state.nodes[implication.split("_")[1]] ).split("\n"))
                  || "";
                return ` ${query} ${imp_str} ${resultStr}`;
              }).join("\n")
          }
          return "";
        }).join("\n\n")
        str += "\n\n" + state.z3feedback[nodeid]
        .filter(msg => msg.rule == "NORULE" || msg.rule == "IMPLIESTARGET")
        .map(({query, implication, result}) => {
          let resultStr = result in state.nodes
            && state.nodes[result].token
            || "";
          let imp_str = implication.split("_")[1] in state.nodes
            && C.clean(kast.format( state.nodes[implication.split("_")[1]] ).split("\n"))
            || "";
          return ` ${query} ${imp_str} ${resultStr}`;
        })
        .join("\n")

          // rule + " " +
          // query + " " +
          // (result in state.nodes && state.nodes[result].term.token || "" ))
          //
          //
          //
        // .map(id => (state.z3feedbackdata || {})[id] || "")
        // .filter(o => !!o)
        // .map(o => {
        //   let rhs        = kast.formatConstraint(o.rhs)
        //   let z3QueryId  = o.queryId
        //   let z3Result   = o.result.term.token
        //   return "result: " + clc.xterm(210)(z3Result)   + "\n"
        //    + "checking:"                             + "\n"
        //    + "    " + rhs + "\n";
        // })
        // .join("\n")
        // return z3feedback;
      return str;
    },
  },
  kcell: {
    view: state => {
      let id = S.term_id(state)
      let k = state.nodes[id];
      let kcell = kast.get(k, "k")
      return kast.format(kcell);
      // return JSON.stringify(kcell, false, 2)
        // .split("~>")
        // .join("\n~>")
    },
  },
  term: {
    view: state => {
      let id = S.term_id(state)
      let k = state.nodes[id];
      let omit = (state.term_omit || [])
      return kast.format(kast.omitCells(k, omit));
    },
  },
  help: {
    view: state => {
      return "TODO";
    },
  },
  log: {
    view: state => {
      return state.error || ""
    },
  }
}

module.exports = view
