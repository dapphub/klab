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

    balanceOf[0]         |-> Burned => Burned + BURN
    balanceOf[dst]       |-> DstBal => DstBal + (value - BURN)
    balanceOf[CALLER_ID] |-> SrcBal => SrcBal - value

iff in range uint256

    SrcBal - value
    DstBal + (value - BURN)
    Burned + BURN

iff

    VCallValue == 0

if

    dst =/= 0
    CALLER_ID =/= 0
    CALLER_ID =/= dst
```
