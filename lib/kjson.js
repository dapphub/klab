const _             = require("lodash");
const kjson         = require("../resources/k.json");

const renderRule  = o => {
    let json = Object.keys(o.term)
      .reduce((obj, path) => _.set(obj.t, path, o.term[path]) && obj, {...kjson})
    json.requires = json.requires.concat(o.requires || [])
    json.ensures = o.ensures
  let compile = obj => typeof obj == "object"
    && (Array.isArray(obj)
      && obj.map(compile).join("\n")
      || Object
        .keys(obj)
        .map(key => `<${key}>${typeof obj[key] == "object" ? "\n" : ""}${compile(obj[key])}${typeof obj[key] == "object" ? "\n" : ""}</${key}>`)
        .join("\n")
        .split("\n")
        .map(s => "  " + s)
        .join("\n")
      )
    || " " + obj + " "
  let generatedTop = compile(json.t);
  let requires     = json.requires.join("\n");
  let ensures      = json.ensures ? '\nensures\n' + json.ensures.join('\n    andBool ') : ''
  let name         = o.name;
  return `// ${name}\nrule\n${generatedTop}\nrequires ${requires} ${ensures}`
}

const renderModule = (rules, hasGas, name) => `requires "../rules.k"
requires "../bin_runtime.k"
${hasGas ? `requires "../gas/${name}gas.k"` : ""}

module ${name.toUpperCase()}
  imports ETHEREUM-SIMULATION
  imports EVM
  imports RULES
  imports BIN_RUNTIME
  ${hasGas ? `imports ${name}GAS` : ""}

${rules.join("\n\n")}

endmodule
`

// o.name
// o.term
// o.requires

module.exports = {
   renderRule,
   renderModule
}
