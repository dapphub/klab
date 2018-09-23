const fs            = require("fs");
const xs            = require("xstream").default;
const {
  testPath,
  ensureDir
} = require("../util.js");

module.exports = config => {

  return function(dump) {
    dump
      .addListener({
        next: msg => {
          if(msg.type == "status") {
            ensureDir("out");
            ensureDir("out/status");
            fs.writeFileSync("out/status/" + msg.name, JSON.stringify(msg, false, 2));
          }
        },
        error: e => console.log(e),
        complete: e => console.log(e)
      })

    return xs.of({});
  }
}
