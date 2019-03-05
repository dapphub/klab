// const {run} = require("@cycle/run")
const xs = require("xstream").default;
const sampleCombine = require("xstream/extra/sampleCombine").default;
const dropRepeats = require("xstream/extra/dropRepeats").default;
const onionify = require('cycle-onionify').default;
const getGasExpr = require('./gas.js');
const link = require("./link.js");
const pull = require("./pull.js");
const view = require("./view.js");
const move = require("./move.js");
const exec = require("./exec.js");


const findCmd = (key, control) => {
  // let _key = Object.keys(control)
  // .find(regexStr => (new RegExp(regexStr)).test(key))
  // .find(regexStr => (new RegExp(regexStr)).test(key))
  return control[key]
}

const main = ({ Dump, CLI, K, Settings, onion }) => {

  const boot$ = Settings
    .map(config => () => ({
      config,
      //
      // keeps information about the connection - remote/local
      // and its status - e.g. connected/error/closed
      connection: config.connection,
      path: [],
      edges: {},
      coedges: {},
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
    .filter(key => key.full == "C-c")
    .map(() => ({type: "stop"}))

  // map cli input to app commands
  const cmd$ = CLI
    .compose(sampleCombine(onion.state$))
  // only if not in cmd_mode
    .filter(([_, state]) => !state.cmd_mode)
  // .map(([key, _]) => key)
  // find the command
    .map(([key, state]) => [key.full, findCmd(key.full, state.config.control)])
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

  // TODO - dump gas on finished state
  // const writeStatus$ = onion.state$
  //   .map(state => state.status)
  //   .filter(status => status == "SUCCESS" || status == "fail")
  //   .compose(dropRepeats())
  //   .compose(sampleCombine(onion.state$))
  //   .map(([_, state]) => state)
  //   .map(state => ({
  //     type: "status",
  //     status: state.status,
  //     name: state.config.name,
  //     proofid: state.config.proofid,
  //     state: state
  //   }))
  const loadedFinished = state => Object.keys(state.finished)
    .map(e => e.split("_"))
    .reduce((a,e) => a.concat(e), [])
    .reduce((a, id) => a && (id in state.nodes), true)
  const writeGas$ = onion.state$
    .filter(state => state.config.name.indexOf("_oog") == -1)
    .filter(state => state.config.name.indexOf("_fail") == -1)
    .filter(loadedFinished)
    .map(state => state.status)
    .filter(status => status == "pass")
    .compose(dropRepeats())
    .compose(sampleCombine(onion.state$))
    .map(([_, state]) => state)
    .map(state => ({
      type: "finish",
      data: {
        gas: {
          name: state.config.name.slice(0, -2) + ".raw.kast.json",
          data: getGasExpr(state)
        },
        pass: {
          name: state.config.proofid,
          data: "yay"
        }
      }
    }))


    // TODO - dirty
    Dump
    .filter(msg => msg.type == "finish")
    .compose(sampleCombine(onion.state$))
    .map(([_, state]) => state)
    .filter(state => state.config.headless)
    .map(state => state.status)
    .filter(status => status == "pass" || status == "fail")
    .addListener({
      next: status => {
        let code = status == "pass" ? 0 : 1;
        process.exit(code)
      },
      error: e => console.log(e),
      complete: e => console.log(e)
    })

    onion.state$
    .filter(state => state.config.name.includes("_oog") || state.status == "fail")
    .filter(state => state.status == "pass" || state.status == "fail")
    .filter(state => state.config.headless)
    .map(state => state.status)
    .addListener({
      next: status => {
        let code = status == "pass" ? 0 : 1;
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
    Dump: writeGas$
  }
}

const wrappedMain = onionify(main);

module.exports = wrappedMain;
