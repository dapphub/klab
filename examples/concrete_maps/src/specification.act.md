```act
behaviour transfer of BurningToken
interface transfer(address dst, uint value)

for all

    SrcBal : uint256
    DstBal : uint256
    Burned : uint256

where

    BURN := 1000

storage

    balanceOf[CALLER_ID] |-> SrcBal => SrcBal - value
    balanceOf[dst]       |-> DstBal => DstBal + (value - BURN)
    balanceOf[0]         |-> Burned => Burned + BURN

iff in range uint256

    value - BURN
    SrcBal - value
    Burned + BURN
    DstBal + (value - BURN)

iff

    VCallValue == 0

if

    dst =/= 0
    CALLER_ID =/= 0
    CALLER_ID =/= dst
```
