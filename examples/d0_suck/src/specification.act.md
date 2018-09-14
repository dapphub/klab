
```
behaviour suck of D0Impl
interface suck(address u, int256 Delta_D)

types

  Root : address
  V_p  : int256
  D_u  : int256
  P    : int256
  V    : int256
  VGas : uint256

storage

  0 |-> Root
  9 |-> uint(V_p)
  10 |-> uint(P)
  11 |-> (uint(V) => V -Int Delta_D)
  12 |-> G
  #hashedLocation("DappHub", 4, u) |-> (uint(D_u) => uint(D_u +Int Delta_D))

iff

  VGas > 300000
  Root == CALLER_ID
  V - Delta_D > 0
  D_u + Delta_D > 0

iff in range int256

  V - Delta_D
  D_u + Delta_D

```
