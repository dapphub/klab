const xs = require("xstream").default;
const sampleCombine = require("xstream/extra/sampleCombine").default;

module.exports = ({ CLI, onion }) => {
  const cmd_mode_enter$ = CLI
    .compose(sampleCombine(onion.state$))
    .filter(([key, state]) => !state.cmd_mode && key == ":")
    .map(([key, _]) => key)
    .map(key => state => {
      return Object.assign({}, state, {cmd_mode: true})
    })
  const cmd_mode_correct$ = CLI
    .compose(sampleCombine(onion.state$))
    .filter(([key, state]) => !!state.cmd_mode && key == "\u007f")
    .map(_ => state => {
      return Object.assign({}, state, {cmd: state.cmd.slice(0, -1)})
    })
  const cmd_mode_write$ = CLI
    .compose(sampleCombine(onion.state$))
    .filter(([key, state]) => !!state.cmd_mode && /^[a-zA-Z0-9\-\_\.\ ]$/.test(key) && key != "\r")
    .map(([key, _]) => key)
    .map(key => state => {
      // TODO - test for valid string
      state.cmd = (state.cmd || "") + key;
      return Object.assign({}, state);
    })
  const cmd_exec$ = CLI
    .compose(sampleCombine(onion.state$))
    .filter(([key, state]) => state.cmd_mode && key == "\r");
  const custom_cmd$ = cmd_exec$
    .map(([_, state]) => ({cmd: state.cmd}))
  const cmd_mode_exec$ = cmd_exec$
    .map(([key, _]) => key)
    .map(key => state => {
      return Object.assign({}, state, {cmd_mode: false, cmd: ""})
    })
  const cmd_mode_exit$ = CLI
    .compose(sampleCombine(onion.state$))
    .filter(([key, state]) => state.cmd_mode && key == "\u001b")
    .map(([key, _]) => key)
    .map(key => state => {
      return Object.assign({}, state, {cmd_mode: false, cmd: ""})
    })

  return {
    ccmd$: xs.merge(
      cmd_mode_enter$,
      cmd_mode_write$,
      cmd_mode_exec$,
      cmd_mode_correct$,
      cmd_mode_exit$
    ),
    crun$: custom_cmd$
  }
}
