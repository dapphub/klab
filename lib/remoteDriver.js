const xs = require("xstream").default;
var WebSocketClient = require('websocket').client;

var connection;
var puffer = [];

module.exports = host => {

  var client = new WebSocketClient();
  client.on('connectFailed', function(error) {
    console.log('Connect Error: ' + error.toString());
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
        client.on('connect', function(connection_) {
          connection = connection_;
          clearPuffer();
          connection.on('error', function(error) {
            listener.error(error);
            console.log("Connection Error: " + error.toString());
          });
          connection.on('close', function() {
            console.log('echo-protocol Connection Closed');
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



