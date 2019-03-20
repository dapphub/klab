```act
behaviour transfer of Token
interface transfer(address To, uint Value)

storage

    #Token.balances[CALLER_ID] |-> FromBal => FromBal - Value
    #Token.balances[To]        |-> ToBal => ToBal + Value

iff in range uint256

    FromBal - Value
    ToBal + Value

iff

    VCallValue == 0

if
    CALLER_ID =/= To
```
