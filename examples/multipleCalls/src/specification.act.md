
```
behaviour calling of easyNest
interface raiseTemp(uint256 x)

types

  CALLEE : address Callee

storage

  0 |-> CALLEE

storage CALLEE

  0 |-> 0 => x

if

  #notPrecompileAddress(CALLEE)
  CALLEE =/= ACCT_ID
  VGas > 300000
  VCallDepth < 1023
  
calls

  Callee.tempDelta
  
behaviour tempDelta of Callee
interface tempDelta(uint256 x)

storage

  0 |-> 0 => x

if

  #rangeUInt(256, BAL)
  VGas > 20411

gas

   VGas => #if ABI_x =/=Int 0 #then VGas -Int 20411 #else VGas -Int 5411 #fi
```


