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
const log = msg => {
  fs.appendFileSync(path.join(KLAB_WD_PATH, `server.log`), msg + "\n")
}

const KLAB_WD_PATH = path.join(process.env.TMPDIR, "klab");
const KLAB_EVMS_PATH = process.env.KLAB_EVMS_PATH;
const KLAB_K_PATH  = process.env.KLAB_K_PATH
                || path.join(KLAB_EVMS_PATH, "./.build/k");

const start = (state, msg, ch) => {
  // const o = proofs[proofid];
  const wd_path = path.join(KLAB_WD_PATH, msg.proofid + "/")
  // const steplog_path = path.join(wd_path, `steps.log`);
  // if(testPath(steplog_path)) fs.unlinkSync(steplog_path)
  const k_path = path.join(KLAB_K_PATH, "k-distribution/target/release/k/bin/kprove");
  const options = [
    "--debugg",
    "--directory",
    path.join(KLAB_EVMS_PATH, ".build/java/"),
    "--z3-executable",
    `./proof-spec.k`,
    "--def-module",
    "VERIFICATION",
    "--output",
    "json",
    "--smt_prelude",
    "/Users/mhhf/dh/k-ds-rpow/exp.smt2" // TODO - abstract away
    // path.join(KLAB_K_PATH, "k-distribution/include/z3/exp.smt2")
  ];
  state.process = spawn(k_path, options, {
    cwd: wd_path
  })

  log("exec " + k_path + " " + options.join(" "))
  ch({
    type: "status",
    data: "parsing",
    proofid: msg.proofid
  })

  state.process.stdout.on('data', data => {
    // TODO - dirty! - parametarize path
    let _path = data.toString().slice(24).trim();
    if(testPath(_path)) {
      log("path " + _path)
      fs.writeFileSync(path.join(wd_path, "OUTPUT_PATH"), _path)
      ch({
        type: "status",
        data: "running",
        proofid: msg.proofid
      })
      ch({
        type: "boot",
        proofid: msg.proofid
      })
    } else {
      log("dunno " + data.toString().trim())
    }
  })
  state.process.stderr.on('data', data => {
    ch({
      type: "error",
      data: data.toString(),
      proofid: msg.proofid
    });
  })
  state.process.on('error', (code) => {
    ch({
      type: "error",
      proofid: msg.proofid
    });
  })
  state.process.on('close', (code) => {
    log("close " + msg.proofid);
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
