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

//Right now the debugg.log path is printed when running kprove

const sha3 = function (str) {
  return keccak('keccak256')
    .update(str)
    .digest('hex')
    .toString()
    .slice(0, 8);
}
let debugging_dir = '';
const KLAB_WD_PATH = path.join(process.env.TMPDIR, "klab");
const KLAB_EVMS_PATH = process.env.KLAB_EVMS_PATH;
let KLAB_K_PATH;
if (process.env.KLAB_K_PATH === undefined) {
    KLAB_K_PATH = path.join(KLAB_EVMS_PATH, "./.build/k")
}
else {
    KLAB_K_PATH = process.env.KLAB_K_PATH;
}
const res_path = path.join(__dirname, "../resources");

const lemmas_tmp_str = fs.readFileSync(path.join(res_path, "verification_tmp.k")).toString();
const lemmas_tmp = _.template(lemmas_tmp_str);

var kprove;
var gen_spec_process;

const halt = (ch) => {
  if(kprove) {
    kill(kprove.pid, "SIGTERM", () => {
      kprove = null;
    });
  }
}

const stop = (ch) => {
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

const ensureDirs = (wd_path, paths) => paths.forEach(p => {
  let p_ = path.join(wd_path, p);
  if(!testPath(p_)) fs.mkdirSync(p_);
})

const gen_spec = ({spec, lemmas, bin_runtime, replay}, cb) => {

}

const readNode = (nodeId) => {
  console.log('node looks like')
  console.log(JSON.parse(fs.readFileSync(path.join(debugging_dir, `/nodes/${nodeId}.json`)).toString()));
  return JSON.parse(fs.readFileSync(path.join(debugging_dir, `/nodes/${nodeId}.json`)).toString());
}

const run = ({spec, lemmas, bin_runtime, replay, inspect, shortcuts, z3prelude}, ch) => {
 const sync_steps = (steps_path) => {
    console.log('syncing steps')
    let steps = fs
      .readFileSync(steps_path)
      .toString()
      .split("\n")
      .filter(s => s != "")
      .map(s => s.split(" "))
    steps.forEach(step => {
       ch({
           type: step[1],
           data: step.slice(2)
       })
//        console.alog('this is step of type ' + step[1])
    }
    );
  }
  if(inspect && testPath(path.join(KLAB_WD_PATH, inspect, `steps.log`))) {
    console.log("syncing");
    ch({type: "sid", data: inspect});
    sync_steps(path.join(KLAB_WD_PATH, inspect, `steps.log`));
    return null;
  }
  let verification = lemmas_tmp({data: lemmas});
  let tasks = Object.keys(ini.parse(spec));
  let spec_file = spec + `\n\n[pgm]\ncompiler: "Solidity"\ncode: "0x${bin_runtime.trim()}"`
  let state = {
    verification,
    spec_file
  };
  let id = sha3(JSON.stringify(state))

  const wd_path = path.join(KLAB_WD_PATH, id + "/")

  ch({type: "sid", data: id});
    console.log("id " + id);
    console.log('replay ' + replay)
    if(replay && testPath(path.join(wd_path, 'debugg-path'))) {
     let steps_path = fs.readFileSync(path.join(wd_path, 'debugg-path')).toString();
     console.log('syncing');
        debugging_dir = steps_path;
     sync_steps(path.join(steps_path, 'debugg.log'));
     return null;
  }

  if(!testPath(KLAB_WD_PATH)) fs.mkdirSync(KLAB_WD_PATH);

  ensureDirs(wd_path, [
    "/", "nodes", "rules", "circc"
  ]);
  const as_path = path.join(res_path, `abstract-semantics.k`);
  const lemmas_path = path.join(res_path, `lemmas.k`);
  fs.writeFileSync(path.join(wd_path, `verification.k`), verification);
  fs.writeFileSync(path.join(wd_path, `/spec.ini`), spec_file);
  fs.copyFileSync(as_path, path.join(wd_path, `abstract-semantics.k`));
  fs.copyFileSync(lemmas_path, path.join(wd_path, `lemmas.k`));

  gen_spec_process = spawn("python3", [
    path.join(res_path, "gen-spec.py"),
    path.join(res_path, "module-tmpl.k"),
    path.join(res_path, "spec-tmpl.k"),
    path.join(wd_path, `spec.ini`),
    `proof`,
    shortcuts
  ].concat(tasks));

  gen_spec_process.stderr.on("data", data => {
    ch({
      type: "error",
      data: data.toString()
    });
  })
  let k_spec_file_buffer = "";
  gen_spec_process.stdout.on('data', (data) => {
    k_spec_file_buffer += data;
  });
  gen_spec_process.on('close', (code) => {
    // write k spec file
    fs.writeFileSync(path.join(wd_path, `proof-spec.k`), k_spec_file_buffer);
    let msgs = [];
    console.log("running k");
//    let steplog_path = path.join(wd_path, `steps.log`);
//    if(testPath(steplog_path)) fs.unlinkSync(steplog_path)
    let k_path = path.join(KLAB_K_PATH, "k-distribution/target/release/k/bin/kprove");
    let options = [
      "--debugg",
      "--directory",
      path.join(KLAB_EVMS_PATH, ".build/java/"),
      "--z3-executable",
      `./proof-spec.k`,
      "--def-module",
      "VERIFICATION",
      "--smt_prelude",
      z3prelude,
    ];
    console.log(k_path + " " + options.join(" "));
    kprove = spawn(k_path, options, {
      cwd: wd_path
    })
    kprove.stdout.on('data', (data, a) => {
        /* Message will tell us where logging is happening. It looks like this
          Debugg logging to path: /path/to/debugg
        */
        //set debugg to the path to debugging (and remove final breakline)
        let msg = data.toString()
        debugging_dir = msg.slice(msg.match("/").index, msg.length-1);
        sync_steps(path.join(debugging_dir, 'debugg.log'));
        fs.writeFileSync(path.join(wd_path, `debugg-path`), debugging_dir)
          /*
      data = data.toString().trim().split(" ");
      let msg = {
        type: data[0],
        data: data.slice(1)
      };
      ch(msg);
*/
    })
    kprove.stderr.on('data', (data, a) => {
      let msg = {
        type: "error",
        data: data.toString()
      };
      // msgs.push(msg);
      ch(msg);
    })
    // kprove.on('close', (code) => {
      // fs.writeFileSync(path.join(wd_path, `steps.json`), JSON.stringify({
      //   msgs
      // }))
    // })
    kprove.on('error', (code) => {
      console.log("error", code);
    })
  })
}

const getnode = (data, ch) => {
    let d_ = data.split(" ");
    console.log('getnode')
    console.log(d_[0])
    console.log(d_[1])
  let node = readNode(d_[1]);
  ch({
    type: "node",
    data: {
      id: d_[1],
      node
    }
  });
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
  let string = getFileExcerpt(filepath, parseInt(from), parseInt(to)).trim()

  return {
    from,
    to,
    filepath,
    string
  };
}

const getrule = (data, ch) => {
  let d_ = data.split(" ");
  let rule = fs.readFileSync(path.join(KLAB_WD_PATH, `/${d_[0]}/rules/${d_[1]}.json`)).toString();
  ch({
    type: "rule",
    data: {
      id: d_[1],
      rule: parseRule(rule)
    }
  });
}

const getcircc = (data, ch) => {
  let d_ = data.split(" ");
  let circc = fs.readFileSync(path.join(KLAB_WD_PATH, `/${d_[0]}/circc/${d_[1]}.json`)).toString();
  ch({
    type: "circcdata",
    data: {
      id: d_[0],
      circc
    }
  });
}

const getz3feedback = (data, ch) => {
  let [proofId, nodeId, dataId] = data.split(" ")
  let [lhsId, rhsId, queryId, resultId] = dataId.split("_")
  let z3feedback = {
    lhs:     readNode(proofId, lhsId),
    rhs:     readNode(proofId, rhsId),
    queryId: queryId,
    query:   readNode(proofId, queryId),
    result:  readNode(proofId, resultId)
  }
  ch({
    type:    "z3feedbackdata",
    proofId: proofId,
    nodeId:  nodeId,
    data:    z3feedback
  });
}

module.exports = (msg, ch) => {
  switch(msg.type) {
    case "run":
      run(msg.data, ch);
      break;
    case "stop":
      stop(ch);
      break;
    case "halt":
      halt(ch);
      break;
    case "getnode":
      getnode(msg.data, ch);
      break;
    case "getrule":
      getrule(msg.data, ch);
      break;
    case "getcircc":
      getcircc(msg.data, ch);
      break;
    case "getz3feedback":
      getz3feedback(msg.data, ch);
      break;
    default:
      console.log("dunno", msg);
  }
}
