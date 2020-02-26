const clc = require('cli-color');
const _ = require('lodash')
const C = require("./constraints.js");
const S = require("./state.js");
const Rule = require("./rule.js");
const kast = require("./kast.js");
const {
  sha3
} = require("../lib/util.js");

const format = require("./format.js")
const {formatMemory, prepareMem} = require("./formatmemory.js");
const { genBehaviour } = require("./behavior.js");
const evmv = require("./evmv.js");
const fs = require("fs");
const {
  getCodeStringFromPc
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
    view: evmv,
    require: "src"
  },
  gas : {
    view: state => {
      let id = S.term_id(state)
      let k = state.nodes[id];
      return kast.format(kast.get(k, "ethereum.evm.callState.gas"));
    },
    require: "src"
  },
  sourcemap: {
    view: state => {
      // SOURCECODE
      let id = S.term_id(state)
      let k = state.nodes[id];
      const {
        pc,
        call_id,
        isCreate
      } = buildDisplayInfo(k);
      let contract_o = state.config.implementations[state.config.v2n[call_id]];
      let contract = state.config.contracts[contract_o.name];
      let src = getCodeStringFromPc(state.config.srcs, contract, parseInt(pc), true, isCreate);
      return src;
    },
    require: "src"
  },
  rule: {
    view: (state) => {
      let id = S.rule(state);
      let rule = state.rules[id] && Rule.formatRule(Rule.parseRule(state.rules[id].term.att)) || '';
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
      let json_string = JSON.stringify(mem);
      let hash = sha3(json_string);
      // fs.writeFileSync("mem/" + hash.slice(0, 20) + ".json", json_string);
      // return kast.format(mem)
      // return JSON.stringify(mem.args.map(l => kast.format(l.args[0]) + " " + kast.format(l.args[1])), false, 2)
      var memStr;
      try {
        memStr = formatMemory(prepareMem(mem));
      } catch(e) {
        memStr = kast.format(mem)
      }
      return memStr
    },
  },
  z3feedback: {
    view: (state) => {
      let nodeid = S.id(state);
      let attempts = state.rule_debug[nodeid] || {};

      let str = Object.keys(attempts)
        .map((ruleid, i) => {
          let rule = state.rules[ruleid]
            && Rule.formatRule(Rule.parseRule(state.rules[ruleid].term.att))
            || ruleid;
            let cs = Object.keys(attempts[ruleid])
              .map(cid => {
                let constraint = state.nodes[cid] || cid;
                let z3 = attempts[ruleid][cid] && attempts[ruleid][cid];
                let z3feedback = Object.keys(z3)
                  .map(z3id => `  |>>> query: ${z3id}  \n  |>>> result: ${state.nodes[z3[z3id]].token}`)
                  .join('\n')
                return `  |> constraints: ${kast.format(constraint).split('\n').join('\n  |> ')}\n${z3feedback}`
              }).join('\n')
          return `Trying Rule ${i}. ${rule}\n${cs}`;
        })
      .join('\n\n')

      return str;
    },
  },
  kcell: {
    view: state => {
      let id = S.term_id(state)
      let k = state.nodes[id];
      let kcell = kast.get(k, "k")
      return kast.format(kcell);
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
  target_unification: {
    view: state => {
      let targettid = state.targett.split("_")[0];
      let termid = S.term_id(state)
      let targett = state.nodes[targettid];
      let term = state.nodes[termid];
      // let omit = (state.term_omit || [])
      let matcher = kast.antimatch(targett, term)
        .map(([path, p, t]) => `${clc.bold(path.join("."))}:\n  c-term: ${t}\n  target: ${p}`)
        .join("\n\n")

      return "\n"+matcher
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
