
```
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

if

  VGas > 300000
  
calls

  Callee.tempDelta
  
behaviour tempDelta of Callee
interface tempDelta(uint256 x)

storage

  0 |-> 0 => x

if

  VGas > 50000

gas

      VGas => #if ABI_x =/=Int 0 #then VGas -Int 20491 #else VGas -Int 5491 #fi
   
calls
   Callee.add
```
We can extract the pc values of internal solidity functions:
```
behaviour add of Callee
interface add(uint256 x, uint256 y) internal

stack
   x : y : JUMPTo : WS => JUMPTo : x + y : WS

gas
   VGas => VGas - 60

iff in range uint256
    x + y

if 
   VGas > 60
   #sizeWordStack (WS) <= 1018
   
```

