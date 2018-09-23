// const {run} = require("@cycle/run")
const xs = require("xstream").default;
const sampleCombine = require("xstream/extra/sampleCombine").default;
const onionify = require('cycle-onionify').default;
const link = require("./link.js");
const pull = require("./pull.js");
const view = require("./view.js");
const modules = require("./modules.js");
const move = require("./move.js");
const exec = require("./exec.js");


const findCmd = (key, control) => {
  // let _key = Object.keys(control)
  // .find(regexStr => (new RegExp(regexStr)).test(key))
  // .find(regexStr => (new RegExp(regexStr)).test(key))
  return control[key]
}

const main = ({ CLI, K, Settings, onion }) => {

  const boot$ = Settings
    .map(config => () => ({
      config,
      //
      // keeps information about the connection - remote/local
      // and its status - e.g. connected/error/closed
      connection: config.connection,
      path: [],
      edges: {},
      nodes: {},
      //
      // steps is a map of ruleid to boolean (true)
      // this indicates wether this rule is considdered a "step"
      // e.g. if next_step is called the current step is forwarded
      // untill the next step is reached
      // TODO - make this part of ruletags
      steps: {},
      //
      // behaviour
      behaviour: {
        frontier: [],
        height: 0,
        root: null
      },
      rules: {},
      crash: {},
      finished: {},
      z3feedback: {},
      show: {
        behaviour: false
      },
      scroll: 0
    }))


  // TODO - do inspect or run depending on flags
  // start the execution
  const load$ = Settings
    .map(config => {
      return Object.assign({
        type: "run"
      }, config)
    });
  // pull data from server
  const {
    pull$,
    remember_pull$
  } = pull({K, onion})
  // terminate
  const stop$ = CLI
    .filter(key => key == "C-c")
    .map(() => ({type: "stop"}))

  // map cli input to app commands
  const cmd$ = CLI
    .compose(sampleCombine(onion.state$))
  // only if not in cmd_mode
    .filter(([_, state]) => !state.cmd_mode)
  // .map(([key, _]) => key)
  // find the command
    .map(([key, state]) => [key, findCmd(key, state.config.control)])
    .filter(([key, cmd]) => !!cmd)
    .map(([key, cmd]) => cmd && ({ key, cmd }))
  // costom command
  const {ccmd$, crun$} = exec({CLI, onion});
  // update state based on user input
  const move$ = xs.merge(cmd$, crun$)
    .compose(move)
  // update state based on K messeges
  let link$ = K
    .compose(link);

  let view$ = view({onion});

  const writeStatus$ = onion.state$
    .filter(state => state.status == "SUCCESS" || state.status == "fail")
    .map(state => ({
      type: "status",
      status: state.status,
      name: state.config.name,
      proofid: state.config.proofid
    }))

    // TODO - dirty
  onion.state$
    .filter(state => state.config.headless)
    .map(state => state.status)
    .filter(status => status == "SUCCESS" || status == "fail")
    .addListener({
      next: status => {
        let code = status == "SUCCESS" ? 0 : 1;
        process.exit(code)
      },
      error: e => console.log(e),
      complete: e => console.log(e)
    })

  return {
    CLI: view$,
    K: xs.merge(
      pull$,
      stop$,
      load$
    ),
    onion: xs.merge(
      move$,
      link$,
      boot$,
      ccmd$,
      remember_pull$
    ),
    Dump: writeStatus$
  }
}

const wrappedMain = onionify(main);

module.exports = wrappedMain;
