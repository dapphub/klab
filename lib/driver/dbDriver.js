const xs = require("xstream").default;
const fs = require("fs");
const ts = require('tail-stream');
const readline = require('readline');
const moment = require('moment');
const path = require("path");
const Rule = require("./../rule.js");
const testPath = path => {
  try {
    fs.accessSync(path, fs.constants.F_OK);
    return true;
  } catch (e) {
    return false;
  }
}

const KLAB_WD_PATH = path.join(process.env.TMPDIR, "klab");
if (!testPath(KLAB_WD_PATH)) fs.makeDirSync(KLAB_WD_PATH);

const logStream = fs.createWriteStream(path.join(KLAB_WD_PATH, 'server.log'), {'flags': 'a'});
const log = msg => {
  let now = moment().format("YY.MM.DD HH-mm-ss")
  console.log(now + ": " + msg);
  logStream.write(now + ": " + msg + "\n")
}

const getBlob = (proofid, blobid, cb) => {
  // const wd_path = path.join(KLAB_WD_PATH, proofid + "/")
  fs.readFile(path.join(KLAB_WD_PATH, `$blobs/${blobid}.json`), (error, content) => {
    let json={};
    try {
      json = JSON.parse(content)
    } catch(e) {
      // TODO - catch it better
      log("badjson  " + proofid + " " + blobid)
    }
    cb(null, json);
  })
}

const getblob = (msg, ch) => {
  getBlob(msg.proofid, msg.blobid, (error, blob) => {
    let o = {
      type: "blob",
      blobid: msg.blobid,
      blob,
      proofid: msg.proofid,
      peer: msg.peer
    };
    if(blob && blob.term && "att" in blob.term) {
      o.rule = Rule.parseRule(blob.term.att);
    }
    ch(o);
  });
}
// const getz3feedback = (msg, ch) => {
//   msg.data
//     .forEach(data => {
//       let [lhsId, rhsId, queryId, resultId] = data.split("_")
//       let z3feedback = {
//         lhs:     getBlob(msg.proofid, lhsId),
//         rhs:     getBlob(msg.proofid, rhsId),
//         queryId: queryId,
//         query:   getBlob(msg.proofid, queryId),
//         result:  getBlob(msg.proofid, resultId)
//       }
//       ch({
//         type:    "z3feedbackdata",
//         proofid: msg.proofid,
//         data:    z3feedback,
//         z3feedbackid: data
//       });
//     })
// }
const subscriptions = {};

const subscribe = (msg, ch) => {
  if(msg.location == "web") {
    const edges_o = JSON.parse(fs.readFileSync(path.join(KLAB_WD_PATH, "boot_" + msg.proofid + ".json")));
    ch({
      type: "bootstrap",
      data: edges_o,
      proofid: msg.proofid,
      peer: msg.peer
    })
    return null;
  }
  // const wd_path = path.join(KLAB_WD_PATH, msg.proofid + "/")
  const log_path = path.join(KLAB_WD_PATH, msg.proofid + ".log");
  if(!testPath(log_path)) return null;

  const initial_content = fs.readFileSync(log_path).toString();
  if(initial_content.length > 0) {
    ch({
      type: "msgs",
      data: initial_content,
      proofid: msg.proofid,
      peer: msg.peer
    })
  }

  const tstream = ts.createReadStream(log_path, {
    beginAt: initial_content.length
  });
  const rl = readline.createInterface({
    input: tstream,
    crlfDelay: Infinity
  });
  subscriptions[msg.peer] = {
    rl,
    tstream
  }
  rl.on("line", line => {
    ch({
      type: "msg",
      data: line.trim(),
      proofid: msg.proofid,
      peer: msg.peer
    })
  })
}
const unsubscribe = (msg, ch) => {
  if(!(msg.peer in subscriptions)) return null;
  subscriptions[msg.peer].rl.close();
  delete subscriptions[msg.peer];
}

module.exports = msgs => {
  return xs.create({
    start: listener => {

      const behaviour = {
        subscribe,
        unsubscribe,
        getblob,
        // getrule,
        // getz3feedback
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
