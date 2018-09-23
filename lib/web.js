import './../bla.scss';

const xs = require("xstream").default;
const sampleCombine = require("xstream/extra/sampleCombine").default;
const flattenSequentially = require("xstream/extra/flattenSequentially").default;
const dropRepeats = require("xstream/extra/dropRepeats").default;
// const onionify = require('cycle-onionify').default;
// const evolve = require("./stateHandler.js");
const {run} = require("@cycle/run");
const {makeHTTPDriver} = require('@cycle/http');
const {makeHistoryDriver} = require("@cycle/history");
const onionify = require('cycle-onionify').default;
const {code, div, label, input, hr, h1, makeDOMDriver} = require('@cycle/dom')
const remoteDriver = require("../lib/webRemoteDriver.js")
const S = require("./state.js");

const move = require("./move.js");
const view = require("./webview.js");

const drivers = {
  HTTP: makeHTTPDriver(),
  DOM: makeDOMDriver('#root'),
  history: makeHistoryDriver()
};

const main = ({HTTP, DOM, onion, history}) => {

  const load$ = xs.of({
    // url: 'http://localhost:8080/hello', // GET method by default
      category: 'boot',
    })
    .compose(sampleCombine(history))
    .map(([e, h]) => {
      e.url = `https://s3.amazonaws.com/dapphub-klab-player/boot_${h.hash.slice(1)}.json`;
      return e;
    })

  const integrate_blob$ = HTTP
    .select('blob')
    .compose(flattenSequentially)
    .map(msg => state => {
      let data = JSON.parse(msg.text)
      state.nodes = {...state.nodes, [msg.request.data]: data};
      return { ...state };
    })

  const boot$ = xs.of({
      //
      // keeps information about the connection - remote/local
      // and its status - e.g. connected/error/closed
      config: {
        // proofid: h.hash.slice(1)
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
      scroll: 0,
  })
  .compose(sampleCombine(history))
  .map(([e, h]) => {e.config.proofid = h.hash.slice(1); return e;})
  .map(e => () => e)

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


  const bootstrap$ = HTTP
    .select('boot')
    .compose(flattenSequentially)
    .map(resp => state => {
      let data = JSON.parse(resp.text)
      state.edges = data.edges;
      state.config = data.config;
      state.initt = data.initt;
      state.path = data.path;
      return {...state};
    })

  let view$ = view({onion});

  const toConstraintRequest$ = onion.state$
    .filter(state => state.path.length > 0
      && !(S.const_id(state) in state.nodes))
    .map(state => ({
      type: "getblob",
      proofid: state.config.proofid,
      blobid: S.const_id(state)
    }))
    .map(s => JSON.stringify(s))
    .compose(dropRepeats())
    .map(s => JSON.parse(s))

  const toTermRequest$ = onion.state$
    .filter(state => state.path.length > 0
      && !(S.term_id(state) in state.nodes))
    .map(state => ({
      type: "getblob",
      proofid: state.config.proofid,
      blobid: S.term_id(state)
    }))
    // TODO - simplify
    .map(s => JSON.stringify(s))
    .compose(dropRepeats())
    .map(s => JSON.parse(s))

  const _pull$ = xs.merge(toTermRequest$, toConstraintRequest$)
    .filter(msg => msg.type == "getblob")
    .map(msg => ({
      category: "blob",
      data: msg.blobid,
      url: `https://s3.amazonaws.com/dapphub-klab-player/${msg.blobid}.json`
    }))

  return {
    DOM: view$,
    HTTP: xs.merge(
      load$,
      _pull$
    ),
    onion: xs.merge(
      integrate_blob$,
      // remember_pull$,
      boot$,
      move$,
      moveBranch$,
      // getconfig$,
      bootstrap$
    )
  }
}

const wrappedMain = onionify(main);

run(wrappedMain, drivers)
