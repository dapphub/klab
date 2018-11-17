```act
behaviour transfer of Token
interface transfer(address To, uint Value)

types

    FromBal : uint256
    ToBal   : uint256

storage

    #Token.balances[CALLER_ID] |-> FromBal => FromBal - Value
    #Token.balances[To]        |-> ToBal => ToBal + Value

iff in range uint256

    FromBal - Value
    ToBal + Value
```
