```act
behaviour run of Calculate
interface run(uint256 A, uint256 B)

macros

    C => A - B
    D => C + B
    E => C + D
    F => D - E


iff in range uint256

    C
    D
    E
    F

iff

    VCallValue == 0

returns F
```
