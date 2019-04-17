```k
syntax Int ::= "MaskLast20"                                            [function]
rule MaskLast20 => 115792089237316195423570985007226406215939081747436879206741300988257197096960 [macro]

rule MaskLast20 &Int A => 0
  requires #rangeAddress(A)
```
