
```act
behaviour add of SafeAdd
interface add(uint256 X, uint256 Y)

iff in range uint256

    X + Y

if

    VGas > 330

returns X + Y
```
