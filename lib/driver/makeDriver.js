const xs = require("xstream").default;
const path = require("path");
const ini = require("ini");
const _ = require("lodash");
const fs = require("fs-extra");
const { spawn } = require('child_process');
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

if(!testPath(KLAB_WD_PATH)) fs.mkdirSync(KLAB_WD_PATH);
const res_path = path.join(__dirname, "../../resources");
const lemmas_tmp_str = fs.readFileSync(path.join(res_path, "verification_tmp.k")).toString();
const lemmas_tmp = _.template(lemmas_tmp_str);

const logStream = fs.createWriteStream(path.join(KLAB_WD_PATH, 'server.log'), {'flags': 'a'});
const log = msg => {
    let d = new Date();
    logStream.write(d.toUTCString()+': '+msg + "\n")
}
const ensureDirs = (wd_path, paths) => paths.forEach(p => {
  let p_ = path.join(wd_path, p);
  if(!testPath(p_)) fs.mkdirSync(p_);
})


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

  ensureDirs(wd_path, [
    "/"
  ]);

  fs.writeFileSync(path.join(KLAB_WD_PATH, msg.proofid + ".log"), "");
  ch({
    type: "status",
    data: "compiling",
    peer: msg.peer
  });
  let prelude = msg.prelude || "";
  let verification = lemmas_tmp({data: msg.lemmas});
  let tasks = Object.keys(ini.parse(msg.spec));
  let spec_file = msg.spec + `\n\n[pgm]\ncompiler: "Solidity"\ncode: "0x${msg.bin_runtime.trim()}"`



  fs.writeFileSync(path.join(wd_path, `prelude.smt2`), prelude);
  fs.writeFileSync(path.join(wd_path, `verification.k`), verification);
  fs.writeFileSync(path.join(wd_path, `spec.ini`), spec_file);
  const as_path = path.join(res_path, `abstract-semantics.k`);
  fs.copyFileSync(as_path, path.join(wd_path, `abstract-semantics.k`));
  const lemmas_path = path.join(res_path, `lemmas.k`);
  fs.copyFileSync(lemmas_path, path.join(wd_path, `lemmas.k`));

  // TODO - multiproof support?
  const gen_spec_process = spawn("python3", [
    path.join(res_path, "gen-spec.py"),
    path.join(res_path, "module-tmpl.k"),
    path.join(res_path, "spec-tmpl.k"),
    path.join(wd_path, `spec.ini`),
    `proof`
  ].concat(tasks));


  // RUN SCRIPT
  const k_path = path.join(KLAB_K_PATH, "k-distribution/target/release/k/bin/kprove");
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
    "(or-else (using-params smt :random-seed 3 :timeout 1000) (using-params smt :random-seed 2 :timeout 2000) (using-params smt :random-seed 1))"
    // "--exclude-smtlib-tags",
    // "expFunc pow256"
  ];
  const options_str = options
    .map(o => o.indexOf(" ") > -1 ? `"${o}"` : o )
    .join(" ")
  let run_bash = `#!/usr/bin/env bash\n${k_path} ${options_str}`
  fs.writeFileSync(path.join(wd_path, "run.sh"), run_bash);
  fs.chmodSync(path.join(wd_path, "run.sh"), "755");

  gen_spec_process.stderr.on("data", data => {
    ch({
      type: "error",
      data: data.toString(),
      peer: msg.peer
    });
  })
  let k_spec_file_buffer = "";
  gen_spec_process.stdout.on('data', (data) => {
    k_spec_file_buffer += data;
  });
  gen_spec_process.on('close', code => {
    // write k spec file
    fs.writeFileSync(path.join(wd_path, `proof-spec.k`), k_spec_file_buffer);
    ch({
      type: "status",
      data: "queue",
      peer: msg.peer
    })
    ch({
      type: "build",
      peer: msg.peer,
      proofid: msg.proofid
    })
    log("build " + msg.proofid)
  })
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
