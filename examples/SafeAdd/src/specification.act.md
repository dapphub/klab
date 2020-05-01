This is safe add:


```act
behaviour add of SafeAdd
interface add(uint256 X, uint256 Y)

iff in range uint256

    X + Y

iff

    CALLVALUE == 0

returns X + Y
```
