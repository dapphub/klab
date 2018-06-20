// const {run} = require("@cycle/run");
const xs = require("xstream").default;
const onionify = require('cycle-onionify').default;
const link = require("./link.js");
const pull = require("./pull.js");
const view = require("./view.js");
const modules = require("./modules.js");
const move = require("./move.js");
const exec = require("./exec.js");
const sampleCombine = require("xstream/extra/sampleCombine").default;

// global control btns
const coreControl = {
  "\\d"    : "branch",
  "N"      : "next_k",
  "P"      : "prev_k",
  "n"      : "next_step",    // evm step
  "p"      : "prev_step",    // evm step
  "\u000e" : "next_branch",
  "\u0010" : "prev_branch"
}

// make all module toggle btns do the 'toggle_module' cmd
const control = Object.keys(modules)
  // get the toggle buttons for each module
  .map(name => modules[name].toggle)
  // reduthe them in to a string of all toggle btns
  .reduce((a, keys) => a.concat(keys), [])
  // add them to the coreControl
  .reduce((a, key) => Object.assign({}, a, {[key]: "toggle_module"}), coreControl)

// get default modules
const default_modules = Object.keys(modules)
  .filter(name => modules[name].default)
  .reduce((a, name) => Object.assign(a, {[name]: true}), {})

const findCmd = key => Object.keys(control)
  .find(regexStr => (new RegExp(regexStr)).test(key) && regexStr)
const cmdMap = cli$ => cli$
  .map(key => [key, findCmd(key)])
  .filter(([key, cmd]) => !!cmd)
  .map(([key, cmd]) => cmd && ({ key, cmd: control[cmd] }))

const main = ({ CLI, K, Settings, onion }) => {

  const boot$ = Settings
    .map(config => () => ({
      config,
      path: [],
      edges: {},
      nodes: {},
      normalnodes: {},
      circc: {},
      rules: {},
      crash: {},
      branch: {},
      halt: {},
      end: {},
      exception: {},
      revert: {},
      z3feedback: {},
      show: default_modules,
      steps: 0
    }))

  // start the execution
  const load$ = Settings
    .map(config => ({
      type: "run",
      data: config
    }));
  // pull data from server
  const pull$ = pull({K, onion})
  // terminate
  const stop$ = CLI
    .filter(key => key == "\u0003")
    .map(() => ({type: "stop"}))

  // map cli input to app commands
  const cmd$ = CLI
    .compose(sampleCombine(onion.state$))
    .filter(([_, state]) => !state.cmd_mode)
    .map(([key, _]) => key)
    .compose(cmdMap)
  // costom command
  const {ccmd$, crun$} = exec({CLI, onion});
  // update state based on user input
  const move$ = xs.merge(cmd$, crun$)
    .compose(move)
  // update state based on K messeges
  let link$ = K.compose(link);

  let view$ = view({onion, Settings});

  return {
    CLI: view$,
    K: xs.merge(pull$, stop$, load$),
    onion: xs.merge(move$, link$, boot$, ccmd$)
  }
}

const wrappedMain = onionify(main);

module.exports = wrappedMain;
