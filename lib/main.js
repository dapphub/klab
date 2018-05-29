// const {run} = require("@cycle/run");
const xs = require("xstream").default;
const onionify = require('cycle-onionify').default;
const evolve = require("./stateHandler.js");
const display = require("./displayHandler.js");


const main = ({ CLI, Remote, Settings, onion }) => {

  const state$ = onion.state$
  .map(state => JSON.stringify(state.show, false, 2))

  let {reducer$, request$} = evolve({CLI, onion, Remote, Settings});
  let {cli$} = display({onion, Settings});

  const terminate$ = CLI
    .filter(key => key == "\u0003")
    .map(() => ({type: "stop"}))


  return {
    CLI: xs.merge(cli$),
    Remote: xs.merge(request$, terminate$),
    onion: reducer$
  }
}

const wrappedMain = onionify(main);

module.exports = wrappedMain;
