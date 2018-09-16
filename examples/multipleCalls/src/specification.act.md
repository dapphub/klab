
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

  CALL_DEPTH < 250
  10 <= CALLEE
  CALLEE =/= ACCT_ID
  #rangeUInt(256, BAL)
  VGas > 300000
```


