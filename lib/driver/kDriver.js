const xs = require("xstream").default;
const path = require("path");
const kill = require('tree-kill');
const { spawn } = require('child_process');
const fs = require("fs");
const moment = require("moment");

const testPath = path => {
  try {
    fs.accessSync(path, fs.constants.F_OK);
    return true;
  } catch (e) {
    return false;
  }
}

const KLAB_WD_PATH = path.join(process.env.TMPDIR, "klab");
if(!testPath(KLAB_WD_PATH)) fs.mkdirSync(KLAB_WD_PATH);
const KLAB_EVMS_PATH = process.env.KLAB_EVMS_PATH;
const KLAB_K_PATH  = process.env.KLAB_K_PATH
                || path.join(KLAB_EVMS_PATH, "./.build/k");

const logStream = fs.createWriteStream(path.join(KLAB_WD_PATH, 'server.log'), {'flags': 'a'});
const log = msg => {
  let now = moment().format("YY.MM.DD HH-mm-ss")
  logStream.write(now + ': ' + msg + "\n")
  console.log(now + ": " + msg);
}

const start = (state, msg, ch) => {
  const wd_path = path.join(KLAB_WD_PATH, msg.proofid + "/")
  const debugg_path = path.join(KLAB_WD_PATH, msg.proofid + ".log");
  const k_path = path.join(KLAB_K_PATH, "k-distribution/target/release/k/bin/kprove");
  var error = "";
  const options = [
    // "--debug",
    "--debugg",
    "--debugg-path",
    (KLAB_WD_PATH == "/" ? KLAB_WD_PATH.slice(0, -1) : KLAB_WD_PATH),
    "--debugg-id",
    msg.proofid,
    "--directory",
    path.join(KLAB_EVMS_PATH, ".build/java/"),
    "--z3-executable",
    `./proof-spec.k`,
    "--def-module",
    "VERIFICATION",
    "--output-tokenize",
    "#And _==K_ <k> #unsigned",
    "--output-omit",
    "<programBytes> <program> <code>",
    "--output-flatten",
    "_Map_ #And",
    "--output",
    "json",
    "--smt_prelude",
    "./prelude.smt2",
    "--z3-tactic",
    "\"(or-else (using-params smt :random-seed 3 :timeout 1000) (using-params smt :random-seed 2 :timeout 2000) (using-params smt :random-seed 1))\""
    // "--exclude-smtlib-tags",
    // "expFunc pow256"
  ];
  state.process = spawn(k_path, options, {
    cwd: wd_path
  })

  log("exec " + k_path + " " + options.join(" "))

  state.process.stdout.on('data', data => {
    // do nothing
    log("out " + data)
  })
  state.process.stderr.on('data', data => {
    log("error " + data + " " + msg.proofid);
    ch({
      type: "error",
      data: data.toString(),
      proofid: msg.proofid
    });
    error += data;
  })
  state.process.on('error', (code) => {
    log("error " + code + " " + msg.proofid);
    ch({
      type: "error",
      proofid: msg.proofid
    });
  })
  state.process.on('close', (code) => {
    // if(error != "") {
    //   fs.writeFileSync(path.join(wd_path, "nodes", "error"), error);
    //   fs.appendFileSync(debugg_path, "error")
    // }
    log("close " + code + " " + msg.proofid);
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
