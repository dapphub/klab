rule #take(N, #padToWidth(N, X) ++ Z ) => X
rule #asWord( #asByteStack( X ) ) => X

rule (#unsigned(A) -Word #unsigned(B)) => #unsigned(A -Int B)
  requires minSInt256 <=Int A andBool A <=Int maxSInt256
  andBool  minSInt256 <=Int B andBool B <=Int maxSInt256
  andBool  minSInt256 <=Int (A -Int B)
  andBool  (A -Int B) <=Int maxSInt256

rule (#unsigned(A) +Int #unsigned(B)) => #unsigned(A +Int B)
  requires minSInt256 <=Int A andBool A <=Int maxSInt256
  andBool  minSInt256 <=Int B andBool B <=Int maxSInt256
  andBool  minSInt256 <=Int (A +Int B)
  andBool  (A +Int B) <=Int maxSInt256

// rule ( (#unsigned ( B ) s<Word 0 ) &Int bool2Word ( A s<Word #unsigned ( A -Int B ) ==Int 0 ) ) => 0
//   requires minSInt256 <=Int A andBool A <=Int maxSInt256
//   andBool  minSInt256 <=Int B andBool B <=Int maxSInt256
//   andBool  minSInt256 <=Int (A -Int B)
//   andBool  (A -Int B) <=Int maxSInt256
//
// rule ( ( 0 s<Word #unsigned ( V1 ) ) &Int bool2Word ( #unsigned ( V -Int V1 ) s<Word V ==Int 0 ) ) => 0
//   requires minSInt256 <=Int A andBool A <=Int maxSInt256
//   andBool  minSInt256 <=Int B andBool B <=Int maxSInt256
//   andBool  minSInt256 <=Int (A -Int B)
//   andBool  (A -Int B) <=Int maxSInt256

rule W0 s<Word W1 => #signed(W0) <Word #signed(W1)

rule #signed(#unsigned(W)) => W
rule #unsigned(#signed(W)) => W

// rule W <Int #if B #then W0 #else W1 #fi => (B impliesBool
rule #hashedLocation("DappHub", BASE, OFFSET OFFSETS) => #hashedLocation("DappHub", keccakIntList(BASE OFFSET),       OFFSETS)
