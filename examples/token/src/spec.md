
```act
behaviour init of Token
interface constructor(string symbol_, string name_, string version_, uint256 chainId_, uint256 _totalSupply)

creates
   string symbol := symbol_
   string name := name_
   uint totalSupply := _totalSupply
   mapping(address=>uint) balanceOf := [CALLER := totalSupply]

```


```act
behaviour transfer of Token
interface transfer(uint256 value, address to)

iff

   CALLVALUE == 0
   value <= balanceOf[CALLER]
   CALLER =/= to

iff in range uint256

   balanceOf[to] + value

   storage

     balanceOf[CALLER] => balanceOf[CALLER] - value
     balanceOf[to]     => balanceOf[to] + value
```
