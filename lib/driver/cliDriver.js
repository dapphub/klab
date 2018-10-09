const fromEvent = require('xstream/extra/fromEvent').default;
const dropRepeats = require("xstream/extra/dropRepeats").default;
const debounce = require("xstream/extra/debounce").default;
const throttle = require("xstream/extra/throttle").default;
const xs = require("xstream").default;
const clear = require("cli-clear");
const kill = require("tree-kill");
const blessed = require("blessed");
var screen, main, cmd, status;

const makeCliDriver = _options => {
  const options = {
    in: true,
    type: "blessed",
    ..._options
  }

  if(options.type == "blessed") {
    screen = blessed.screen({
      smartCSR: true,
      terminal: 'xterm-256color',
      fullUnicode: true
    });
    main = blessed.box({
      parent: screen,
      left: 0,
      top: 0,
      width: '100%',
      height: '100%-2',
      scrollable: true,
      style: {
        bg: 234
      }
    });
    cmd = blessed.box({
      parent: screen,
      left: 0,
      bottom: 0,
      valign: "bottom",
      width: '100%'
    });
    status = blessed.box({
      parent: screen,
      left: 0,
      bottom: 1,
      valign: "bottom",
      width: "100%"
    })
    screen.append(main);
  }

  const stdin = process.stdin;
  if(options.in) {
    stdin.setRawMode( true );
    stdin.resume();
    stdin.setEncoding( 'utf8' );
  }

  const cliDriver = stdout => {
    const norepeat$ = stdout
      .compose(dropRepeats())
    const throttled$ = norepeat$
      .compose(throttle(100))
    const debounced$ = norepeat$
      .compose(debounce(100))

    xs.merge(throttled$, debounced$)
      .compose(dropRepeats())
      .addListener({
        next: vdom => {
          if(options.type === "blessed") {
            main.setContent(vdom.main);
            cmd.setContent(vdom.cmd);
            status.setContent(vdom.status);
            screen.render();
          } else {
            console.log(vdom.main)
          }
        }
      })

    return xs.create({
      start: (listener) => {
        process.stdout.on('resize', () => {
          listener.next("");
        })
        stdin.on("keypress", (key, yy) => {
          listener.next(yy || {
            full: key,
            sequence: key
          });
        })
      },
      stop: () => {}
    })
  }
  return cliDriver;
}

module.exports = makeCliDriver;
