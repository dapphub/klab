const fs            = require("fs");
const xs            = require("xstream").default;
const path = require("path");
const {
  testPath,
  ensureDir
} = require("../util.js");

const KLAB_OUT = process.env.KLAB_OUT || "out";

module.exports = config => {
  return function(dump) {
    return xs.create({
      start: listener => {
        dump
        .addListener({
          next: msg => {
            ensureDir(path.join(KLAB_OUT, msg.type))
            fs.writeFileSync(path.join(KLAB_OUT, msg.type, msg.name), msg.data);
            listener.next(msg);
          },
          error: e => console.log(e),
          complete: e => console.log(e)
        })
      },
      stop: () => {}
    });
  }
}
