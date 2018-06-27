const xs = require("xstream").default;
const local = require("../local_tmp.js");



module.exports = msgs => {

  return xs.create({
    start: listener => {
      msgs.addListener({
        next: msg => {
          local(msg, (resp) => {
            resp.peer = msg.peer;
            listener.next(resp);
          });
        }
      })
    },
    stop: () => {}
  })
}
