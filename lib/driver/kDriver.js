const xs = require("xstream").default;
const path = require("path");
const kill = require('tree-kill');
const { spawn } = require('child_process');
const fs = require("fs");

const testPath = path => {
  try {
    fs.accessSync(path, fs.constants.F_OK);
    return true;
  } catch (e) {
    return false;
  }
}

const KLAB_WD_PATH = path.join(process.env.TMPDIR, "klab");
const KLAB_EVMS_PATH = process.env.KLAB_EVMS_PATH;
const KLAB_K_PATH  = process.env.KLAB_K_PATH
                || path.join(KLAB_EVMS_PATH, "./.build/k");

const logStream = fs.createWriteStream(path.join(KLAB_WD_PATH, 'server.log'), {'flags': 'a'});
const log = msg => {
    logStream.write(msg + "\n")
}

const start = (state, msg, ch) => {
  const wd_path = path.join(KLAB_WD_PATH, msg.proofid + "/")
  const k_path = path.join(KLAB_K_PATH, "k-distribution/target/release/k/bin/kprove");
  const options = [
    // "--debug",
    "--debugg",
    "--debugg-path",
    (wd_path[wd_path.length - 1] == "/" ? wd_path.slice(0, -1) : wd_path),
    "--directory",
    path.join(KLAB_EVMS_PATH, ".build/java/"),
    "--z3-executable",
    `./proof-spec.k`,
    "--def-module",
    "VERIFICATION",
    "--output-tokenize",
    "<wordStack> <k>",
    "--output-omit",
    "<programBytes>",
    "--output",
    "json",
    "--smt_prelude",
    "./prelude.smt2"
  ];
  state.process = spawn(k_path, options, {
    cwd: wd_path
  })

  log("exec " + k_path + " " + options.join(" "))

  state.process.stdout.on('data', data => {
    // do nothing
  })
  state.process.stderr.on('data', data => {
    log("error " + data + " " + msg.proofid);
    ch({
      type: "error",
      data: data.toString(),
      proofid: msg.proofid
    });
  })
  state.process.on('error', (code) => {
    log("error " + code + " " + msg.proofid);
    ch({
      type: "error",
      proofid: msg.proofid
    });
  })
  state.process.on('close', (code) => {
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
