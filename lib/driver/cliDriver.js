const fromEvent = require('xstream/extra/fromEvent').default;
const dropRepeats = require("xstream/extra/dropRepeats").default;
const xs = require("xstream").default;
const clear = require("cli-clear");
const kill = require("tree-kill");
const blessed = require("blessed");
const screen = blessed.screen({
    smartCSR: true,
    terminal: 'xterm-256color',
    fullUnicode: true
  });

const stdin = process.stdin;
stdin.setRawMode( true );
stdin.resume();
stdin.setEncoding( 'utf8' );

const main = blessed.box({
    parent: screen,
    left: 0,
    top: 0,
    width: '100%',
    height: '100%-1'
});
const cmd = blessed.box({
    parent: screen,
    left: 0,
    bottom: 0,
  valign: "bottom",
  width: '100%'
});
screen.append(main);
// screen.append(cmd);

function cliDriver(stdout) {
  stdout
    .compose(dropRepeats())
    .addListener({
    next: vdom => {
      main.setContent(vdom.main);
      cmd.setContent(vdom.cmd);
      screen.render();
      // console.log(i)
    }
  })

  return xs.create({
    start: (listener) => {
      stdin.on("data", key => {
        listener.next(key);
      })
    },
    stop: () => {

    }
  })
}

module.exports = cliDriver;
