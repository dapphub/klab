### Mom storage

```k
syntax Int ::= "#Mom.kid0" [function]
rule #Mom.kid0 => 0

syntax Int ::= "#Mom.kid1" [function]
rule #Mom.kid1 => 1

syntax Int ::= "#Mom.kid2" [function]
rule #Mom.kid2 => 2

syntax Int ::= "#Mom.kid3" [function]
rule #Mom.kid3 => 3

syntax Int ::= "#Mom.kid4" [function]
rule #Mom.kid4 => 4
```

### Kid storage

```k
syntax Int ::= "#Kid.parent" [function]
rule #Kid.parent => 0
```

### KidToken storage

```k
syntax Int ::= "#KidToken.parent" [function]
rule #KidToken.parent => 0

syntax Int ::= "#KidToken.name" [function]
rule #KidToken.name => 1

syntax Int ::= "#KidToken.DOMAIN_SEPARATOR" [function]
rule #KidToken.DOMAIN_SEPARATOR => 2
```
