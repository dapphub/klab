const xs = require("xstream").default;
const fs = require("fs");
const ts = require('tail-stream');
const readline = require('readline');
const path = require("path");
const Rule = require("./../rule.js");

const KLAB_WD_PATH = path.join(process.env.TMPDIR, "klab");


const logStream = fs.createWriteStream(path.join(KLAB_WD_PATH, 'server.log'), {'flags': 'a'});
const log = msg => {
    logStream.write(msg + "\n")
}
const getBlob = (proofid, blobid) => {
  const wd_path = path.join(KLAB_WD_PATH, proofid + "/")
  const content = fs.readFileSync(path.join(wd_path, `nodes/${blobid}.json`)).toString();
  let json={};
  try {
    json = JSON.parse(content)
  }catch(e) {
    // TODO - catch it better
    log("badjson  " + proofid + " " + blobid)
  }
  return json;
}

const getblob = (msg, ch) => {
  let blob = getBlob(msg.proofid, msg.blobid);
  let o = {
    type: "blob",
    blobid: msg.blobid,
    blob,
    proofid: msg.proofid,
    peer: msg.peer
  };
  if("att" in blob.term) {
    o.rule = Rule.parseRule(blob.term.att);
  }
  ch(o);
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
  const wd_path = path.join(KLAB_WD_PATH, msg.proofid + "/")
  const tstream = ts.createReadStream(path.join(wd_path, "debugg.log"));
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
