Storage locations for token
```k
syntax Int ::= "#Token.balances" "[" Int "]" [function]
// -----------------------------------------------
// doc: The token balance of `$0`
rule #Token.balances[A] => #hashedLocation("Solidity", 0, A)
```
