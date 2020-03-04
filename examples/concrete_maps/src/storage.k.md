```k
syntax Int ::= "#BurningToken.balanceOf" "[" Int "]" [function]
rule #BurningToken.balanceOf[A] => #hashedLocation("Solidity", 0, A)
```
