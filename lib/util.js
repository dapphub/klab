const clc    = require("cli-color");
const fs     = require("fs");
const path   = require("path");
const locale = require("./locale.js");
const keccak = require("keccak");
const { execSync }  = require('child_process');
const sha3 = function (str) {
  return keccak('keccak256')
    .update(str)
    .digest('hex')
    .toString()
}
const KLAB_OUT     = process.env.KLAB_OUT || "out";

const testPath = _path => {
  try {
    fs.accessSync(_path, fs.constants.F_OK);
    return true;
  } catch (e) {
    return false;
  }
}

const ensureDir  = dir => {
  try {
    if(!testPath(dir)) fs.mkdirSync(dir);
  } catch(e) {
    // do nothing
  }
}


const getStatus = hash =>
  testPath(path.join(KLAB_OUT, "accept", hash)) && 'accept'
    || testPath(path.join(KLAB_OUT, "timeout", hash)) && 'timeout'
    || testPath(path.join(KLAB_OUT, "reject", hash)) && 'reject'
    || '????'

const ensureDirs = dirs => {
  dirs.forEach(ensureDir);
}

const revert = msg  => console.error(clc.xterm(locale.color.ERROR)(msg)) || process.exit(1);
const read   = path => fs.readFileSync(path).toString();
const warn   = str  => process.stderr.write(clc.xterm(214)(str) +"\n")

const render = (str, obj) => str.replace(/\{\{([^\}]*)\}\}/g, (_, match) => obj[match] || "")

//TODO: refactor and get rid of these repetitions
const mapInterface = (is, str) => {
  (is || []).forEach(([t, n]) => {
    str = str.replace(new RegExp("([^\\w\.]|^)" + n + "([^\\w]|$)", "g"), (a, b, c, d) => {
      return b + "ABI_" + n + c;
    })
  })
  return str;
}

const toK = str => str
  .replace(/ \+ /g, " +Int ")
  .replace(/ \- /g, " -Int ")
  .replace(/ \* /g, " *Int ")
  .replace(/ \/ /g, " /Int ")
  .replace(/ \> /g, " >Int ")
  .replace(/ \< /g, " <Int ")
  .replace(/ \<\= /g, " <=Int ")
  .replace(/ \>\= /g, " >=Int ")
  .replace(/ \=\= /g, " ==Int ")
  .replace(/ \=\/\= /g, " =/=Int ")
  .replace(/ and /g, " andBool ")
  .replace(/ or /g, " orBool ")
  .replace(/ not /g, " notBool ")
  .replace(/uint\(/g, "#unsigned(")
//  .replace(/bool\(/g, "bool2Word(")

const getKlabHEAD = () => {
  return execSync(`git rev-parse HEAD`, {
    cwd: path.join(__dirname, '..'),
    encoding: 'utf8'
  });
}

const getProjectHEAD = () => {
  var project_HEAD = "";

  if(testPath('.git')) {
    project_HEAD = execSync(`git rev-parse HEAD`, {
      encoding: 'utf8'
    }).trim();
  }

  return project_HEAD;
}

const getBuildId = () => {

  const klab_HEAD     = getKlabHEAD();
  const project_HEAD     = getProjectHEAD();

  const build_hash = sha3(JSON.stringify({
    project_HEAD, // e.g. current k-dss git HEAD
    klab_HEAD,    // e.g. klab master HEAD
  })).slice(0, 20);

  return build_hash;
}


module.exports = {
  ensureDirs,
  ensureDir,
  testPath,
  render,
  revert,
  read,
  sha3,
  warn,
  mapInterface,
  toK,
  getBuildId,
  getKlabHEAD,
  getProjectHEAD,
  getStatus
};
