```act
behaviour callMe of Int
interface callMe(int x)

returns #unsigned(x)
```
asd
```act
behaviour callIt of Uint
interface callIt(uint y)

types

    Int : address IntLike

storage

    0 |-> Int

storage Int

iff

   VCallDepth < 1024

calls

    Int.callMe
```
