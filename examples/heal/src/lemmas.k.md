```k
// subui
// lemmas for sufficiency
rule A -Word #unsigned(B) => A -Int B
  requires #rangeUInt(256, A)
  andBool #rangeSInt(256, B)
  andBool #rangeUInt(256, A -Int B)

 rule A -Word #unsigned(B) <=Int A => minUInt256 <=Int A -Int B
   requires #rangeUInt(256, A)
   andBool #rangeSInt(256, B)
   andBool 0 <=Int B

 rule A <Int A -Word #unsigned(B) => minUInt256 <=Int A -Int B
   requires #rangeUInt(256, A)
   andBool #rangeSInt(256, B)
   andBool B <Int 0

rule A -Word #unsigned(B) <Int A => maxUInt256 <Int A -Int B
   requires #rangeUInt(256, A)
   andBool #rangeSInt(256, B)
   andBool B <Int 0

```
