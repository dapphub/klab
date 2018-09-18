import './../bla.scss';

const xs = require("xstream").default;
// const onionify = require('cycle-onionify').default;
// const evolve = require("./stateHandler.js");
const {run} = require("@cycle/run");
const onionify = require('cycle-onionify').default;
const remoteDriver = require("../lib/webRemoteDriver.js")
const {code, div, label, input, hr, h1, makeDOMDriver} = require('@cycle/dom')

const move = require("./move.js");
const pull = require("./pull.js");
const link = require("./link.js");
const view = require("./webview.js");

const drivers = {
  K: remoteDriver("127.0.0.1:8080"),
  // Settings: () => xs.of(config),
  DOM: makeDOMDriver('#root')
};

const PROOFID = "f7347706f3b1003c778aa343617f79313f06c901fdfca93d684bdfca13ddae1a";

const main = ({DOM, K, onion}) => {

  const load$ = xs.of({
    type: "getconfig",
    proofid: PROOFID
  }, {
    type: "sync",
    proofid: PROOFID,
    location: "web"
  });

  const boot$ = xs.of(() => ({
      //
      // keeps information about the connection - remote/local
      // and its status - e.g. connected/error/closed
      config: {
        proofid: PROOFID
      },
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

  const move$ = DOM
    .select("button")
    .events("click")
    .map(e => ({cmd: e.currentTarget.className}))
    .compose(move)

  const moveBranch$ = DOM
    .select(".node.branching")
    .events("click")
    .map(e => ({cmd: "branch " + e.ownerTarget.dataset.index}))
    .compose(move)

  const {
    pull$,
    remember_pull$
  } = pull({K, onion})

  const getconfig$ = K
    .filter(msg => msg.type == "config")
    .map(msg => state => {
      console.log("got config");
      state.config = msg.config;
      return state;
    })

  let link$ = K
    .compose(link);

  let view$ = view({onion});


  return {
    DOM: view$,
    K: xs.merge(
      pull$,
      load$
    ),
    onion: xs.merge(
      remember_pull$,
      boot$,
      link$,
      move$,
      moveBranch$,
      getconfig$
    )
  }
}

const wrappedMain = onionify(main);

run(wrappedMain, drivers)
