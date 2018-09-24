const xs = require("xstream").default;
const path = require("path");
const kill = require('tree-kill');
const { spawn } = require('child_process');
const fs = require("fs");
const moment = require("moment");
const clc    = require("cli-color");
const {
  testPath
} = require("../util.js");

const KLAB_WD_PATH = path.join(process.env.TMPDIR, "klab");
if(!testPath(KLAB_WD_PATH)) fs.mkdirSync(KLAB_WD_PATH);
const KLAB_EVMS_PATH = process.env.KLAB_EVMS_PATH;
const KLAB_K_PATH  = process.env.KLAB_K_PATH
                || path.join(KLAB_EVMS_PATH, "./.build/k");

const logStream = fs.createWriteStream(path.join(KLAB_WD_PATH, 'server.log'), {'flags': 'a'});
const log = (pid, msg) => {
  let now = moment().format("MM.DD HH-mm-ss")
  logStream.write(now + ' ' + pid + " " + msg + "\n")
  console.log(clc.xterm(244)(now) + " " + clc.xterm(244)(pid.slice(0, 8)) + "  " + msg);
}

const start = (state, msg, ch) => {
  const wd_path = path.join(KLAB_WD_PATH, msg.proofid + "/")
  const debugg_path = path.join(KLAB_WD_PATH, msg.proofid + ".log");
  const spec_name = fs.readdirSync(path.join(wd_path, "specs"))[0]
  var error = "";
  state.process = spawn(path.join(wd_path, "run.sh"), [path.join(wd_path, "specs", spec_name), msg.proofid], {
    cwd: wd_path
  })

  log(msg.proofid, "./run.sh " + spec_name + "  "  + msg.proofid)

  state.process.stdout.on('data', data => {
    // do nothing
    log(msg.proofid, "out " + data)
  })
  state.process.stderr.on('data', data => {
    log(msg.proofid, "error " + data);
    ch({
      type: "error",
      data: data.toString(),
      proofid: msg.proofid
    });
    error += data;
  })
  state.process.on('error', (code) => {
    log(msg.proofid, "error " + code);
    ch({
      type: "error",
      proofid: msg.proofid
    });
  })
  state.process.on('close', (code) => {
    fs.appendFileSync(debugg_path, "0 close \n")
    log(msg.proofid, "close " + code);
    ch({
      type: "stop",
      proofid: msg.proofid
    })
    state.process = null;
  })
}
const stop = (state, msg, ch) => {
  kill(state.process.pid, "SIGTERM", () => {
    state.process = null;
  });
}

module.exports = msgs => {
  return xs.create({
    start: listener => {

      const state = {
        process: null   // Current kprove instance
      }
      const behaviour = {
        // start a new proof
        start,
        // stops the k process
        stop
      };

      msgs.addListener({
        next: msg => {
          if(msg.type in behaviour) behaviour[msg.type](state, msg, resp => {
            listener.next(resp);
          })
        }
      })
    },
    stop: () => {}
  })
}
