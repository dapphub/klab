// TODO migrate prelude rules export from klab build into here and make sure make driver does it the same way.
// make sure the new structure works with server/client
const fs            = require("fs");
const path          = require("path");
const marked        = require("marked");
const {
  testPath,
  read,
  revert
}                   = require("./util.js")

const makePrelude = config => {
  const prelude_path = config.src.smt_prelude || "./src/prelude.smt2.md";
  if(!testPath(prelude_path)) {
    console.error(`ERR: smt prelude file not found at ${prelude_path}`);
    process.exit();
  }

  let smt_prelude = marked.lexer(read(prelude_path))
    .filter(e => e.type === "code")
    .map(e => e.text)
    .join("\n") + "\n"

  fs.writeFileSync("out/prelude.smt2", smt_prelude)
}

const makeRules = config => {

  const rules_template = rules => `requires "edsl.k"
requires "evm.k"

module RULES
    imports EVM
    imports EDSL

${rules.join("\n\n")}

endmodule
`

  const rules = marked.lexer(config.src.rules
    .join("\n"))
    .filter(e => e.type === "code")
    .map(e => e.text)
    .join("\n")

  const rules_k = rules_template([
    fs.readFileSync(path.join(__dirname, "../resources/rules.k.tmp")).toString(),
    rules
  ])

  fs.writeFileSync("out/rules.k", rules_k);
}

const makeRunScript = () => {
  fs.copyFileSync(path.join(__dirname, "../resources/run.sh"), `out/run.sh`);
}

module.exports = {
  makePrelude,
  makeRules,
  makeRunScript
}
