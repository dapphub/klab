const xs = require("xstream").default;
const {span, ul, li, button, i, td, tr, th, table, code, div, label, input, hr, h1, makeDOMDriver} = require('@cycle/dom')

const r = s => " ".repeat(4 - s.length) + s
const hex = n => n.toString(16).length % 2 == 0 ? n.toString(16) : "0" + n.toString(16);
const S = require("./state.js");
const {
  getCodeStringFromPc,
} = require("../lib/srchandlerweb.js");

const { genBehaviour } = require("./behavior.js");

let behaviourView = behaviour => div(`.node${behaviour.branching ? ".branching" : ""}${behaviour.active ? ".active" : ".hide"}`, {dataset: {index: behaviour.index}}, [
  span(".index", behaviour.index),
  span(".deltaC", behaviour.deltaC || ""),
  ul(".branch", behaviour.children.map(behaviourView))
])


module.exports = ({onion}) => {

  const vdom$ = onion.state$
    .map(state => {
      if(!state.path || state.path.length == 0 || !state.config) {
        return code("loading..." + state.path + " " + !!state.config  )
      } else {
        let id = S.term_id(state)

        const {
          stack_flatt_kast,
          pc,
          storage_object,
          call_id
        } = state.nodes[id];

        let storage_str = JSON.stringify(storage_object, false, 2)

        let spec_o = state.config.rule_meta;
        let contract_o = state.config.implementations[spec_o.v2n[call_id]];
        let contract = state.config.contracts[contract_o.name];

        let src = getCodeStringFromPc(state.config.srcs, contract, parseInt(pc), true);

        let inst = contract.pc_to_inst_map[pc];
        let from = Math.max(0, inst - 5);
        let to   = Math.min(contract.instructions.length, from + 10);
        let pc_inst_mnemonic_table = contract.instructions
          .slice(from, to)
          .map((s, i) => {
            let isActive = from + i === inst;
            let rowClass = isActive ? ".highlighted" : "";

            let pc = hex(contract.inst_to_pc[from + i])
            let hinst = hex(from + i)
            let mnemonic = s;

            return tr(rowClass, [
              td(pc),
              td(hinst),
              td(mnemonic)
            ])
          })

        let stack_table = stack_flatt_kast
          .map((v, i) => {
            return tr([
              td(span([hex(i)])),
              td(span([v]))
            ])
          })

        let behaviour = genBehaviour(state);


        return div([
          div(".navigation", [
            button(".prev_branch", [
              i(".fas.fa-fast-backward")
            ]),
            button(".prev_k", [
              i(".fas.fa-step-backward"),
            ]),
            button(".next_k", [
              i(".fas.fa-step-forward"),
            ]),
            button(".next_branch", [
              i(".fas.fa-fast-forward")
            ])
          ]),
          div(".player", [
            table(".position", [
              tr([
                th("PC"),
                th("ID"),
                th("Mnemonic")
              ])
            ].concat(pc_inst_mnemonic_table)),
            table(".stack",
              [
                tr([
                  th("Key"),
                  th("Value")
                ])
              ].concat(stack_table)
            ),
            code(".src", src),
            code("STORAGE\n" + storage_str),
            behaviourView(behaviour)
          ])
        ])
      }
    });

  return vdom$
}
