```
behaviour transfer of Token
interface transfer(address To, uint Value)

types

    FromBal : uint256
    ToBal   : uint256

storage

    #hashedLocation("Solidity", 0, CALLER_ID) |-> FromBal => FromBal - Value
    #hashedLocation("Solidity", 0, To)        |-> ToBal   => ToBal + Value

iff in range uint256

    FromBal - Value
    ToBal + Value
```
