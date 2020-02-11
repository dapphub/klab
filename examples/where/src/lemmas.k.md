```k
rule A -Word B <=Int A => #rangeUInt(256, A -Int B)
  requires #rangeUInt(256, A) andBool #rangeUInt(256, B)
```

