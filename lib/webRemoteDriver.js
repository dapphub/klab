const xs = require("xstream").default;
var W3CWebSocket = require('websocket').w3cwebsocket;

var puffer = [];

// TODO - node or browser flag
// define a different constructor for browser
module.exports = host => {

  console.log("host: " + host);
  var client = new W3CWebSocket(`ws://${host}`, 'echo-protocol', {
    closeTimeout: 50000,
    maxReceivedMessageSize: 1024 * 1024 * 50
  });
  client.onerror = function(error) {
    console.log('Connect Error: ' + JSON.stringify(error, false, 2));
  };

  return function (msgs) {

    const clearPuffer = () => {
      if(puffer.length > 0) {
        puffer.reverse();
        puffer.forEach(p => {
          client.send(JSON.stringify(p));
        })
        puffer = [];
      }
    }

    msgs.addListener({
      next: msg => {
        if(client.readyState == 1) {
          clearPuffer();
          client.send(JSON.stringify(msg));
        } else {
          puffer.push(msg)
        }
      }
    })


    return xs.create({
      start: listener => {
        client.onopen = function(connection_) {
          console.log("open connection");
          clearPuffer();
          // connection.on('error', function(error) {
          //   listener.error(error);
          //   console.log("Connection Error: " + error.toString());
          // });
          client.onclose = function() {
            console.log('echo-protocol Connection Closed');
          };
          client.onmessage = function(message) {
            let msg = JSON.parse(message.data);
            listener.next()
          };
        };
      },
      stop: () => {
      }
    })
  }

}



