## Datatypes
### Messages
All messages are json objects containing the attribute *type*,
declaring the type of the message and an arbitrary amount of
other atributes depending on the type of the message.


### Client to Server:
* run
* stop
* close
* getblob
* getrule
* getz3feedback
* inspect




## Backend Drivers

### kDriver

Manages the k process.

Gets msgs:
* start     - start a new proof
* stop      - stops the proof process

Sends msgs:
* status    - sends the current status
* boot      - start the data streaming process
* error     - report error
* stop      - the process has topped

### makeDriver

Manages the proof build process.

Gets msgs:
* make

Sends msgs:
* error
* ok

### dbDriver

Manages the access to data

Gets msgs:
* getblob
* getnode
* getrule
* getz3feedback
* subscribe

Sends msgs:
* blob
* node
* rule
* z3feedbackdata
* msg
* end

