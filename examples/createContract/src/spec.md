```act
behaviour create of Mom
interface create()

types

    KID    : address Kid

creates storage KID

    #Kid.parent |-> ACCT_ID

storage

    #Mom.child |-> 0 => KID

if

    #newAddr(ACCT_ID, Nonce_Mom) == KID

iff

    VCallValue == 0
    VCallDepth <Int 1024
```
