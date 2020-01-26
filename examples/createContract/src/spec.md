```act
behaviour create of Mom
interface create()

types

    Parent : address
    KID    : address Kid

creates storage KID

    #Kid.parent |-> Parent

storage

    #Mom.child |-> _ => KID

iff

    VCallValue == 0
```
