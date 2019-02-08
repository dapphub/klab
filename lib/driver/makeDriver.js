const xs = require("xstream").default;
const path = require("path");
const ini = require("ini");
const _ = require("lodash");
const fs = require("fs-extra");
const { spawnSync } = require('child_process');
const {
  testPath,
  ensureDir
} = require("../util/util.js");

const KLAB_WD_PATH = path.join(process.env.TMPDIR, "klab");
const KLAB_EVMS_PATH = process.env.KLAB_EVMS_PATH;
const KLAB_K_PATH  = process.env.KLAB_K_PATH
                || path.join(KLAB_EVMS_PATH, "./.build/k");

ensureDir(KLAB_WD_PATH)
const res_path = path.join(__dirname, "../../resources");

const logStream = fs.createWriteStream(path.join(KLAB_WD_PATH, 'server.log'), {'flags': 'a'});
const log = msg => {
    let d = new Date();
    logStream.write(d.toUTCString()+': '+msg + "\n")
}

const make = (msg, ch) => {
  const wd_path = path.join(KLAB_WD_PATH, msg.proofid + "/")
  const debugg_path = path.join(KLAB_WD_PATH, msg.proofid + ".log");
  log("make " + msg.proofid)

  if(!msg.force && testPath(debugg_path)) {
    ch({
      type: "status",
      data: "syncing",
      proofid: msg.proofid,
      peer: msg.peer
    });
    ch({
      type: "syncing",
      proofid: msg.proofid,
      peer: msg.peer
    });
    return null;
  } else if(msg.force && testPath(debugg_path)) {
    fs.unlinkSync(debugg_path)
    if(msg.clean) {
      fs.removeSync(wd_path);
    }
  }

  ensureDir(wd_path)
  ensureDir(path.join(wd_path, "specs"))

  fs.writeFileSync(path.join(KLAB_WD_PATH, msg.proofid + ".log"), "");
  ch({
    type: "status",
    data: "compiling",
    peer: msg.peer
  });

  fs.writeFileSync(path.join(wd_path, `config.json`), JSON.stringify(msg));

  fs.writeFileSync(path.join(wd_path, `prelude.smt2`), msg.smt_prelude);
  fs.writeFileSync(path.join(wd_path, `rules.k`), msg.rules);
  fs.writeFileSync(path.join(wd_path, "specs", msg.name), msg.spec);
  fs.copyFileSync(path.join(res_path, "run.sh"), path.join(wd_path, "run.sh"))

  ch({
    type: "status",
    data: "queue",
    peer: msg.peer
  })
  ch({
    type:    "build",
    peer:    msg.peer,
    proofid: msg.proofid
  })
  log("build " + msg.proofid)
}


module.exports = msgs => {
  return xs.create({
    start: listener => {

      const behaviour = {
        // make a new proof
        make
      };

      msgs.addListener({
        next: msg => {
          if(msg.type in behaviour) behaviour[msg.type](msg, resp => {
            listener.next(resp);
          })
        }
      })
    },
    stop: () => {}
  })
}
