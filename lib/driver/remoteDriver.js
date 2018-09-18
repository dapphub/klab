const xs = require("xstream").default;
const WebSocketClient = require('websocket').client;

var connection;
var puffer = [];

// TODO - node or browser flag
// define a different constructor for browser
module.exports = host => {

  var client = new WebSocketClient({
    closeTimeout: 50000
  });

  client.connect(`ws://${host}`, 'echo-protocol');

  return function (msgs) {

    const clearPuffer = () => {
      if(puffer.length > 0) {
        puffer.reverse();
        puffer.forEach(p => {
          connection.sendUTF(JSON.stringify(p));
        })
        puffer = [];
      }
    }

    msgs.addListener({
      next: msg => {
        if(msg.type == "stop") process.exit();
        if(connection) {
          clearPuffer();
          connection.sendUTF(JSON.stringify(msg));
        } else {
          puffer.push(msg)
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
          connection.on('message', function(message) {
            listener.next(JSON.parse(message.utf8Data))
          });
        });
      },
      stop: () => {
      }
    })
  }

}



