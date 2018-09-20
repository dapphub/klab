const xs = require("xstream").default;
const WebSocketClient = require('websocket').client;
const pure = require("../pure.js");

var connection;
var out_puffer = [];
var in_puffer = [];
var in_puffer_index = 0;
var in_puffer_running = false;

// TODO - node or browser flag
// define a different constructor for browser
module.exports = host => {

  var client = new WebSocketClient({
    closeTimeout: 50000,
    maxReceivedMessageSize: 1024 * 1024 * 50
  });

  client.connect(`ws://${host}`, 'echo-protocol');

  return function (msgs) {

    const clearPuffer = () => {
      if(out_puffer.length > 0) {
        out_puffer.reverse();
        out_puffer.forEach(p => {
          connection.sendUTF(JSON.stringify(p));
        })
        out_puffer = [];
      }
    }

    msgs.addListener({
      next: msg => {
        if(msg.type == "stop") process.exit();
        if(connection) {
          clearPuffer();
          connection.sendUTF(JSON.stringify(msg));
        } else {
          out_puffer.push(msg)
        }
      }
    })

    return xs.create({
      start: listener => {
        client.on('connectFailed', function(error) {
          console.log("failed" + error);
          listener.next({
            type: "connection",
            status: "failed"
          })
        });
        client.on('connect', function(connection_) {
          listener.next({
            type: "connection",
            status: "connected"
          })
          connection = connection_;
          clearPuffer();
          connection.on('error', function(error) {
            console.log("failed" + error);
            listener.next({
              type: "connection",
              status: "error",
              error
            });
          });
          connection.on('close', function(reason) {
            console.log("close" + reason );
            listener.next({
              type: "connection",
              status: "closed"
            });
          });
          let next = () => {
            listener.next(in_puffer[in_puffer_index]);
            in_puffer_index++;
            if(in_puffer_index < in_puffer.length) {
              setTimeout(next, 1);
            } else {
              in_puffer_running = false;
            }
          }
          connection.on('message', function(message) {
            let msg = JSON.parse(message.utf8Data)
            if(msg.type === "msgs") {
              let queue = msg.data
                .split("\n")
                .map(line => pure({
                  type: "msg",
                  data: line.trim()
                }))
                .filter(msg => msg.type !== "unknown");
              in_puffer = in_puffer.concat(queue)

              in_puffer_running = true;
              next();
            } else if(
                 in_puffer.length > 0
              && in_puffer_index < in_puffer.length
              && msg.type == "msg"
            ) {
              in_puffer.push(msg);
              if(!in_puffer_running) {
                in_puffer_running = true;
                next();
              }
            } else {
              if(msg.type == "msg") {
                listener.next(pure(msg))
              } else {
                listener.next(msg)
              }
            }
          });
        });
      },
      stop: () => {
      }
    })
  }

}



