```act
syntax Int ::= "#Vat.dai" "[" Int "]" [function]
rule #Vat.dai[A] => #hashedLocation("Solidity", 0, A)

syntax Int ::= "#Vat.sin" "[" Int "]" [function]
rule #Vat.sin[A] => #hashedLocation("Solidity", 1, A)

syntax Int ::= "#Vat.debt" [function]
rule #Vat.debt => 2

syntax Int ::= "#Vat.vice" [function]
rule #Vat.vice => 3
```
