const fs            = require("fs");
const xs            = require("xstream").default;
const path = require("path");
const {
  testPath,
  ensureDir
} = require("../util/util.js");

const KLAB_OUT = process.env.KLAB_OUT || "out";

module.exports = config => {
  return function(dump) {
    return xs.create({
      start: listener => {
        dump
        .addListener({
          next: msg => {
            Object.keys(msg.data)
              .forEach(type => {
                let blob = msg.data[type];
                ensureDir(path.join(KLAB_OUT, type))
                fs.writeFileSync(path.join(KLAB_OUT, type, blob.name), blob.data);
              })
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
