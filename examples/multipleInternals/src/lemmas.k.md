
```k
rule #padToWidth(32, #asByteStack(#unsigned(V))) => #asByteStackInWidth(#unsigned(V), 32)
    requires #rangeSInt(256, V)

rule ACCTCODE in SetItem( 1 )
                 SetItem ( 2 )
                 SetItem ( 3 )
                 SetItem ( 4 )
                 SetItem ( 5 )
                 SetItem ( 6 )
                 SetItem ( 7 )
                 SetItem ( 8 )
                 => false
     requires 10 <=Int ACCTCODE

rule V modInt X => V
     requires V <Int X
```
