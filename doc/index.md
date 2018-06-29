
Klab is split in two parts: the frontend and the backend.

The backend deals with the k framework [1], a python script
by runtimeverification that compiles the k rules [2] and a custom
caching layer. It is written for multipeer access and communicates
over websockets.
The frontend is a cli tool to navigate over the generated statespace.


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


# Ressources
[1] - [https://github.com/kframework/k/tree/state-dumping](https://github.com/kframework/k/tree/state-dumping)
[2] - https://github.com/runtimeverification/verified-smart-contracts/blob/master/resources/gen-spec.py
