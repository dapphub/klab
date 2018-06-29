var WebSocketServer = require('websocket').server;
var http = require('http');
const _ = require("lodash");
const xs = require("xstream").default;

const server = http.createServer(function(request, response) {
  // console.log((new Date()) + ' Received request for ' + request.url);
  response.writeHead(404);
  response.end();
});
server.listen(8080, function() {
  // console.log((new Date()) + ' Server is listening on port 8080');
});

wsServer = new WebSocketServer({
  httpServer: server,
  // You should not use autoAcceptConnections for production
  // applications, as it defeats all standard cross-origin protection
  // facilities built into the protocol and the browser.  You should
  // *always* verify the connection's origin and decide whether or not
  // to accept it.
  autoAcceptConnections: false
});

module.exports = function() {

  return (resp) => {
    const connections = {
    };

    resp.addListener({
      next: msg => {
        if(msg.peer in connections) {
          let _msg = _.omit(msg, ["peer"]);
          connections[msg.peer].sendUTF(JSON.stringify(_msg));
        }
      }
    })

    return xs.create({
      start: listener => {
        wsServer.on('request', function(request) {
          let _c = connections[request.key] = request.accept('echo-protocol', request.origin);
          // console.log((new Date()) + ' Connection accepted.');
          listener.next({
            type: "connected",
            peer: request.key
          });
          _c.on('message', function(message, a, b) {
            let msg = JSON.parse(message.utf8Data);
            msg.peer = request.key;
            listener.next(msg);
          });
          _c.on('close', function(reasonCode, description) {
            listener.next({
              type: "disconnected",
              peer: request.key
            });
            // console.log((new Date()) + ' Peer ' + _c.remoteAddress + ' disconnected.');
          });
        });
      },
      stop: () => {

      }
    })
  }

}
