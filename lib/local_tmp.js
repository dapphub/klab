// TODO - ch should be a generic channel, not peer specific
const _ = require("lodash");
const fs = require("fs");
const keccak = require("keccak");
const { spawn } = require('child_process');
const kill = require('tree-kill');
const ini = require("ini");
const path = require("path");
const testPath = path => {
  try {
    fs.accessSync(path, fs.constants.F_OK);
    return true;
  } catch (e) {
    return false;
  }
}

const peers = {};
const proofs = {};
const proof_subscriptions = {};
// proof task queue
var queue = [];
var state = "IDLE";
var kprove;


const sha3 = function (str) {
  return keccak('keccak256')
    .update(str)
    .digest('hex')
    .toString()
}

const KLAB_WD_PATH = path.join(process.env.TMPDIR, "klab");
const KLAB_EVMS_PATH = process.env.KLAB_EVMS_PATH;
const KLAB_K_PATH  = process.env.KLAB_K_PATH
                || path.join(KLAB_EVMS_PATH, "./.build/k");

const res_path = path.join(__dirname, "../resources");

const lemmas_tmp_str = fs.readFileSync(path.join(res_path, "verification_tmp.k")).toString();
const lemmas_tmp = _.template(lemmas_tmp_str);

const log = msg => {
  fs.appendFileSync(path.join(KLAB_WD_PATH, `server.log`), msg + "\n")
}
const ensureDirs = (wd_path, paths) => paths.forEach(p => {
  let p_ = path.join(wd_path, p);
  if(!testPath(p_)) fs.mkdirSync(p_);
})
const readNode = (proofId, nodeId) => {
  return JSON.parse(fs.readFileSync(path.join(KLAB_WD_PATH, `${proofId}/nodes/${nodeId}.json`)).toString());
}
const getFileExcerpt = (path, from, to) => fs
  .readFileSync(path)
  .toString()
  .split("\n")
  .slice(from - 1, to)
  .filter(l => l != "")
  .join("\n");
const parseRule = ruleString => {
  const pos_regex = /Location\((\d+)\,\d+\,(\d+)\,\d+\)/;
  const src_regex = /Source\(Source\(([^\)]+)\)/;
  const location = ruleString.match(pos_regex);
  const filepath = ruleString.match(src_regex)[1];
  const from = location[1];
  const to = location[2];
  // TODO - assert filepath exist
  let string = getFileExcerpt(filepath, parseInt(from), parseInt(to)).trim()

  return {
    from,
    to,
    filepath,
    string
  };
}
const make = (proofid, cb) => {
  const o = proofs[proofid];
  let verification = lemmas_tmp({data: o.lemmas});
  let tasks = Object.keys(ini.parse(o.spec));
  let spec_file = o.spec + `\n\n[pgm]\ncompiler: "Solidity"\ncode: "0x${o.bin_runtime.trim()}"`

  const wd_path = path.join(KLAB_WD_PATH, proofid + "/")

  // if(o.replay && testPath(path.join(wd_path, `steps.log`))) {
  //   log("syncing")
  //   sync_steps(path.join(wd_path, `steps.log`), ch);
  //   return null;
  // } else {
  //   log("compiling...")
  // }

  if(!testPath(KLAB_WD_PATH)) fs.mkdirSync(KLAB_WD_PATH);
  ensureDirs(wd_path, [
    "/", "nodes", "rules"
  ]);

  fs.writeFileSync(path.join(wd_path, `verification.k`), verification);
  fs.writeFileSync(path.join(wd_path, `spec.ini`), spec_file);
  const as_path = path.join(res_path, `abstract-semantics.k`);
  fs.copyFileSync(as_path, path.join(wd_path, `abstract-semantics.k`));
  const lemmas_path = path.join(res_path, `lemmas.k`);
  fs.copyFileSync(lemmas_path, path.join(wd_path, `lemmas.k`));

  const gen_spec_process = spawn("python3", [
    path.join(res_path, "gen-spec.py"),
    path.join(res_path, "module-tmpl.k"),
    path.join(res_path, "spec-tmpl.k"),
    path.join(wd_path, `spec.ini`),
    `proof`
  ].concat(tasks));

  gen_spec_process.stderr.on("data", data => {
    // ch({
    //   type: "error",
    //   data: data.toString()
    // });
  })
  let k_spec_file_buffer = "";
  gen_spec_process.stdout.on('data', (data) => {
    k_spec_file_buffer += data;
  });
  gen_spec_process.on('close', code => {
    // write k spec file
    fs.writeFileSync(path.join(wd_path, `proof-spec.k`), k_spec_file_buffer);
    cb(null, code);
  })
}

const exec = proofid => {
  log("proof " + proofid);
  const o = proofs[proofid];
  const wd_path = path.join(KLAB_WD_PATH, proofid + "/")
  const steplog_path = path.join(wd_path, `steps.log`);
  if(testPath(steplog_path)) fs.unlinkSync(steplog_path)
  const k_path = path.join(KLAB_K_PATH, "k-distribution/target/release/k/bin/kprove");
  const options = [
    "--debugg",
    "--directory",
    path.join(KLAB_EVMS_PATH, ".build/java/"),
    "--z3-executable",
    `./proof-spec.k`,
    "--def-module",
    "VERIFICATION",
    "--smt_prelude",
    "/Users/mhhf/dh/k-ds-rpow/exp.smt2" // TODO - abstract away
    // path.join(KLAB_K_PATH, "k-distribution/include/z3/exp.smt2")
  ];
  kprove = spawn(k_path, options, {
    cwd: wd_path
  })
  log("exec " + k_path + " " + options.join(" "))
  kprove.stdout.on('data', dataArr => {
    // TODO - wrap semantics
    if(testPath(dataArr.toString().slice(24).trim())) {
      log("path " + dataArr.toString().slice(24).trim())
    } else {
      log("dunno " + dataArr.toString().slice(24).trim())
    }
    // dataArr.toString().split("\n").forEach(data => {
    //   data = data.trim().split(" ");
    //   let msg = {
    //     type: data[0],
    //     data: data.slice(1)
    //   };
    //   fs.appendFileSync(path.join(wd_path, `steps.log`), data.join(" ") + "\n")
    //   log("data " + JSON.stringify(msg))
    //   proof_subscriptions[proofid](msg);
    // })
  })
  kprove.stderr.on('data', data => {
    let msg = {
      type: "error",
      data: data.toString()
    };
    log("error " + "proofid" + " " + data.toString());
    proof_subscriptions[proofid](msg);
  })
  kprove.on('error', (code) => {
    let msg = {
      type: "error",
      data: data.toString()
    };
    log("error " + proofid + " " + code);
  })
  kprove.on('close', (code) => {
    log("close " + proofid);
    state = "IDLE";
    kprove = null;
    next();
  })
}

const next = () => {
  if(state == "IDLE" && queue.length > 0) {
    state = "RUNNING"
    let proofid = queue[0];
    queue = queue.slice(1);
    exec(proofid);
  }
}




// TODO - send bulk?
const sync_steps = (steps_path, ch) => {
  let steps = fs
    .readFileSync(steps_path)
    .toString()
    .split("\n")
    .filter(s => s != "")
    .map(s => s.split(" "))

  steps.forEach(step => ch({
    type: step[0],
    data: step.slice(1)
  }));
}










const close = (_, ch) => {
  if(kprove) {
    kill(kprove.pid, "SIGTERM", () => {
      kprove = null;
      state = "IDLE";
      next();
    });
  }
}
// TODO - this can only happen in local mode
// TODO - rename to exit
const stop = (_, ch) => {
  if(kprove) {
    kill(kprove.pid, "SIGTERM", () => {
      kprove = null;
      process.exit();
    });
  } else {
    kprove = null;
    process.exit();
  }
}
const run = (o, ch) => {
  // TODO change inspect here in to sid which is computed on the client, so sync and inspect become the same
  proof_subscriptions[o.proofid] = ch;
  if(o.replay && testPath(path.join(KLAB_WD_PATH, o.proofid, `steps.log`))) {
    // console.log("syncing");
    sync_steps(path.join(KLAB_WD_PATH, o.proofid, `steps.log`), ch);
    return null;
  }

  // Assert proofid
  let _proofid = sha3(JSON.stringify({
    lemmas: o.lemmas,
    bin_runtime: o.bin_runtime,
    spec: o.spec
  }))
  if(_proofid !== o.proofid) {
    // TODO - cleanup peer and return error
    log("proofids dont match")
    return null;
  }

  log("compiling " + o.proofid)
  proofs[o.proofid] = Object.assign({
    status: "compiling"
  }, o)
  make(o.proofid, code => {
    proofs[o.proofid].status = "waiting"
    proofs[o.proofid].code = code;
    queue.push(o.proofid);
    next();
  });
}
const getblob = (msg, ch) => {
  let blob = readNode(msg.proofid, msg.blobid);
  ch({
    type: "blob",
    blobid: msg.blobid,
    blob
  });
}
const getnode = (msg, ch) => {
  let d_ = msg.data.split(" ");
  let node = readNode(d_[0], d_[1]);
  ch({
    type: "node",
    data: {
      id: d_[1],
      node
    }
  });
}
const getrule = (msg, ch) => {
  let d_ = msg.data.split(" ");
  let blob_path = path.join(KLAB_WD_PATH, `/${d_[0]}/rules/${d_[1]}.json`)
  let rule = fs.readFileSync(blob_path).toString();
  ch({
    type: "rule",
    data: {
      id: d_[1],
      rule: parseRule(rule)
    }
  });
}
const getz3feedback = (msg, ch) => {
  msg.data
    .forEach(data => {
      let [lhsId, rhsId, queryId, resultId] = data.split("_")
      let z3feedback = {
        lhs:     readNode(msg.proofid, lhsId),
        rhs:     readNode(msg.proofid, rhsId),
        queryId: queryId,
        query:   readNode(msg.proofid, queryId),
        result:  readNode(msg.proofid, resultId)
      }
      ch({
        type:    "z3feedbackdata",
        proofid: msg.proofid,
        data:    z3feedback,
        z3feedbackid: data
      });
    })
}
const inspect = (msg, ch) => {
  if(testPath(path.join(KLAB_WD_PATH, msg.proofid, `steps.log`))) {
    sync_steps(path.join(KLAB_WD_PATH, inspect, `steps.log`), ch);
    return null;
  }
}

const behaviour = {
  run,
  stop,
  close,
  getnode,
  getblob,
  getrule,
  getz3feedback,
  inspect
};

module.exports = (msg, ch) => {
  log(JSON.stringify(msg));
  msg.type in behaviour && behaviour[msg.type](msg, ch)
  || console.log(msg.type)
}
