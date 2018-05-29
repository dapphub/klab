const fromEvent = require('xstream/extra/fromEvent').default;
const xs = require("xstream").default;
const clear = require("cli-clear");
const kill = require("tree-kill");

const stdin = process.stdin;
stdin.setRawMode( true );
stdin.resume();
stdin.setEncoding( 'utf8' );

function cliDriver(stdout) {
  stdout.addListener({
    next: i => {
      clear();
      console.log(i)
    },
    error: err => console.error(err),
    complete: () => console.log('completed')
  })

  return xs.create({
    start: (listener) => {
      stdin.on("data", key => {
        if(key === "\u0003") {
          listener.next(key)
          // process.exit();
        }
        listener.next(key);
      })
    },
    stop: () => {

    }
  })
}

module.exports = cliDriver;
