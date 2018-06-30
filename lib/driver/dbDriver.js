const xs = require("xstream").default;
const fs = require("fs");
const ts = require('tail-stream');
const readline = require('readline');
const path = require("path");

const KLAB_WD_PATH = path.join(process.env.TMPDIR, "klab");

const getBlob = (proofid, nodeid) => {
  const wd_path = path.join(KLAB_WD_PATH, proofid + "/")
  return JSON.parse(fs.readFileSync(path.join(wd_path, `nodes/${nodeid}.json`)).toString());
}

const getblob = (msg, ch) => {
  let blob = getBlob(msg.proofid, msg.blobid);
  ch({
    type: "blob",
    blobid: msg.blobid,
    blob,
    proofid: msg.proofid,
    peer: msg.peer
  });
}
const getrule = (msg, ch) => {
  const wd_path = path.join(KLAB_WD_PATH, msg.proofid + "/")
  let blob_path = path.join(wd_path, `/nodes/${msg.rule}.json`)
  let rule = fs.readFileSync(blob_path).toString();
  ch({
    type: "rule",
    data: {
      id: d_[1],
      rule: parseRule(rule)
    },
    proofid: msg.proofid
  });
}
const getz3feedback = (msg, ch) => {
  msg.data
    .forEach(data => {
      let [lhsId, rhsId, queryId, resultId] = data.split("_")
      let z3feedback = {
        lhs:     getBlob(msg.proofid, lhsId),
        rhs:     getBlob(msg.proofid, rhsId),
        queryId: queryId,
        query:   getBlob(msg.proofid, queryId),
        result:  getBlob(msg.proofid, resultId)
      }
      ch({
        type:    "z3feedbackdata",
        proofid: msg.proofid,
        data:    z3feedback,
        z3feedbackid: data
      });
    })
}
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
        getblob,
        getrule,
        getz3feedback,
        unsubscribe
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
