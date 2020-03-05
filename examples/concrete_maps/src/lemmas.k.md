# Lemmas

## Underflow

Solidity underflow checking implies logicial no-underflow condition.

```k
rule A -Word B <=Int A => #rangeUInt(256, A -Int B)
  requires #rangeUInt(256, A) andBool #rangeUInt(256, B)
```

## Storage Mappings

```k
rule keccakIntList(A B) ==K 78338746147236970124700731725183845421594913511827187288591969170390706184117 => false
requires A =/=Int 0

rule keccakIntList(A B) ==K 78338746147236970124700731725183845421594913511827187288591969170390706184117 => false
requires 0 =/=Int A

rule 78338746147236970124700731725183845421594913511827187288591969170390706184117 ==K keccakIntList(A B) => false
requires A =/=Int 0

rule 78338746147236970124700731725183845421594913511827187288591969170390706184117 ==K keccakIntList(A B) => false
requires 0 =/=Int A
```


