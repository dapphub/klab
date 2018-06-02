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

const sha3 = function (str) {
  return keccak('keccak256')
    .update(str)
    .digest('hex')
    .toString()
    .slice(0, 8);
}

const KLAB_WD_PATH = path.join(process.env.TMPDIR, "klab");
const KLAB_K_PATH = process.env.KLAB_K_PATH;
const KLAB_EVMS_PATH = process.env.KLAB_EVMS_PATH;
const res_path = path.join(__dirname, "../resources");

const lemmas_tmp_str = fs.readFileSync(path.join(res_path, "verification_tmp.k")).toString();
const lemmas_tmp = _.template(lemmas_tmp_str);

var kprove;
var gen_spec_process;

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


const run = ({spec, lemmas, bin_runtime, replay}, ch) => {
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
  if(replay && testPath(path.join(wd_path, `steps.json`))) {
    console.log("syncing");
    let steps = JSON.parse(fs.readFileSync(path.join(wd_path, `/steps.json`)));
    steps.msgs.forEach(step => ch(step));
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
    `proof`
  ].concat(tasks));

  gen_spec_process.stderr.on("data", data => {
    ch({
      type: "error",
      data: data.toString()
    });
  })
  gen_spec_process.stdout.on('data', (data) => {
    fs.writeFileSync(path.join(wd_path, `proof-spec.k`), data);
  });
  gen_spec_process.on('close', (code) => {
    let msgs = [];
    console.log("run");
    kprove = spawn(path.join(KLAB_K_PATH, "k-distribution/target/release/k/bin/kprove"), [
      // "--debug",
      "--directory",
      path.join(KLAB_EVMS_PATH, ".build/java/"),
      "--z3-executable",
      `./proof-spec.k`,
      "--def-module",
      "VERIFICATION",
      "--z3-tactic",
      "(and-then qfnra-nlsat default)"
    ],{
      cwd: wd_path
    })
    kprove.stdout.on('data', (data, a) => {
      data = data.toString().trim().split(" ");
      let msg = {
        type: data[0],
        data: data.slice(1)
      };
      msgs.push(msg);
      ch(msg);
    })
    kprove.stderr.on('data', (data, a) => {
      let msg = {
        type: "error",
        data: data.toString()
      };
      msgs.push(msg);
      ch(msg);
    })
    kprove.on('close', (code) => {
      fs.writeFileSync(path.join(wd_path, `steps.json`), JSON.stringify({
        msgs
      }))
    })
    kprove.on('error', (code) => {
      console.log("error", code);
    })
  })
}

const getnode = (data, ch) => {
  let d_ = data.split(" ");
  let node = JSON.parse(fs.readFileSync(path.join(KLAB_WD_PATH, `${d_[0]}/nodes/${d_[1]}.json`)).toString());
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

module.exports = (msg, ch) => {
  switch(msg.type) {
    case "run":
      run(msg.data, ch);
      break;
    case "stop":
      stop(ch);
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
    default:
      console.log("dunno", msg);
  }
}
