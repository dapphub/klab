### Mom storage

```k
syntax Int ::= "#Mom.kid0" [function]
rule #Mom.kid0 => 0

syntax Int ::= "#Mom.kid1" [function]
rule #Mom.kid1 => 1

syntax Int ::= "#Mom.kid2" [function]
rule #Mom.kid2 => 2
```

### Kid storage

```k
syntax Int ::= "#Kid.parent" [function]
rule #Kid.parent => 0

syntax Int ::= "#Kid.ID" [function]
rule #Kid.ID => 1
```
