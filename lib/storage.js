const path          = require("path")
const marked        = require("marked");
const Config        = require("../lib/config.js");
const {
  testPath,
  read,
  revert,
  render,
  warn,
  mapInterface
}                   = require("../lib/util.js")
const KLAB_OUT      = process.env.KLAB_OUT || "out";
const config_json   = JSON.parse(read("./config.json"));
const config        = Config(config_json);
const rule_paths    = config.src.rules;
const raw_rules     = rule_paths.map(p => read(p)).join("\n\n")
const rules         = marked
  .lexer(raw_rules)
  .filter(block => block.type === "code")
  .map(block => block.text)
  .join("\n\n")

// returns the storage def object
// Array of "lines"

const getBlock = block => {
  // clean Terminals
  let T = str => str
         .slice(1, -1)
         .replace(/\[|\]|\(|\)|\.|\#|\$|\-|\_/g, e => "\\" + e)
  // memorize Nonterminals
  let NT = "(.*)"
  // decide if `str` is terminal
  let isT = str => /^\"[^\"]*\"$/.test(str)
  // test syntax
  let regex = "^"
    + block.find(str => /^syntax/.test(str))
    .split(" ")
    // remove k junk
    .slice(3, -1)
    // reduce all Terminals and Nonterminals to a regex
    .reduce((a, e, i) => a
      // whitespace possible between args
      + (i > 0 ? "\\s*" : "")
      // add terminal or memorize NT
      + (isT(e) ? T(e) : NT)
      , "")
    + "$";
  let test = str => (new RegExp(regex)).test(str)
  let getArgs = str => str.match(new RegExp(regex)).slice(1)
  let act = str => (block
    .find(line => /^\s*\/\/\sact\:/.test(line)) || "")
    .replace(/^\s*\/\/\sact\:\s*/, "")
    .replace(/\$(\d+)/g, (_, d) => getArgs(str)[d])
  let doc = str => (block
    .find(line => /^\s*\/\/\sdoc\:/.test(line)) || "")
    .replace(/^\s*\/\/\sdoc\:\s*/, "")
    .replace(/\$(\d+)/g, (_, d) => getArgs(str)[d])
  return {
    test,
    getArgs,
    act,
    doc
  }
}

const storage_def = rules
  .split(/\n(\ \t)*\n/)
  .filter(block => block !== undefined)
  .filter(block => /^(\s*\/\/[^\n]*\n)*syntax/.test(block))
  .map(block => block.split("\n"))
  .reduce((a, block) => {
    return a.concat(getBlock(block));
  }, [])
const getStorageDef = str => storage_def.find(def => def.test(str))


const collisionCheck = act => {
    if (act.storage) {
      Object.keys(act.storage).forEach(
          subjectName => {
              let alphaKeys = Object.keys(act.storage[subjectName]).map(
                  key => {
                      let key_ = /^(\d|\#)/.test(key) ? key : `#${subjectName == "ACCT_ID" ? act.subject : subjectName}.${key}`
                      let def = getStorageDef(key_)
                      let args = def ? def.getArgs(key_) : null;
                      //Check for alpha equivalence by removing the arguments entirely
                      let entry = key;
                      if (args) {
                      args.forEach(arg => {
                                   entry = key.replace('[' + arg + ']', '');
                         })
                      }
                      return [entry, args];
                  })
              let indices = alphaKeys.map(s => s[0])

              let collisions = {};
              indices.forEach(key => {
                  collisions[key] = alphaKeys.filter(e => e[0] == key && e[1]).map(s => s[1].join())
              })
              Object.keys(collisions).forEach(key => {
                  if (collisions[key].length > 1) {
                      let ineq1 = "(" + collisions[key]
                          .map(s => mapInterface(act.interface, s))
                          .join(' =/=Int ') + ")"
                      let ineq2 = "(" + collisions[key]
                          .map(s => mapInterface(act.interface, s))
                          .reverse()
                          .join(' =/=Int ') + ")"
                      if (act.if) {
                          if (!(act.if.includes(ineq1) || act.if.includes(ineq2))) {
                              if (collisions[key].length > 1) {
                                  warn(`Warning! Potential collision in storage index ${key}. Arguments ${collisions[key].join(' and ')} may be equal.`)
                              }
                          }
                      } else {
                          warn(`Warning! Potential collision in storage index ${key}. Arguments ${collisions[key].join(' and ')} may be equal.`)
                      }
                  }
              })
          })
    }
}

module.exports = {
    collisionCheck,
    getStorageDef
}
