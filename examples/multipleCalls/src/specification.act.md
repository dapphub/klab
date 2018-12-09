
```act
behaviour calling of easyNest
interface raiseTemp(uint256 x)

types

  CALLEE : address Callee

storage

  0 |-> CALLEE

storage CALLEE

  0 |-> 0 => x

iff
  VCallDepth < 1024

calls

  Callee.tempDelta

behaviour tempDelta of Callee
interface tempDelta(uint256 x)

storage

  0 |-> 0 => x
   
calls
   Callee.add
```
We can extract the pc values of internal solidity functions:
```
behaviour add of Callee
interface add(uint256 x, uint256 y) internal

stack
   x : y : JUMPTo : WS => JUMPTo : x + y : WS

iff in range uint256
    x + y

if 
   #sizeWordStack (WS) <= 1018
   
```

