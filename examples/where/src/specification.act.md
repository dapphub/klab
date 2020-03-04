```act
behaviour run of Calculate
interface run(uint256 A, uint256 B)

for all

    Result : uint256

where

    C := A - B
    D := C * B
    E := D + A

storage

    result |-> Result => E

iff in range uint256

    C
    D
    E

iff

    VCallValue == 0

returns E
```
