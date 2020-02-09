### create

```act
behaviour create_kid0 of Mom
interface create_kid0()

types

    KID    : address Kid

creates storage KID

    #Kid.parent |-> ACCT_ID

storage

    #Mom.kid0 |-> 0 => KID

if

    #newAddr(ACCT_ID, Nonce_Mom) == KID

iff

    VCallValue == 0
    VCallDepth <Int 1024
```

### create assembly

```act
behaviour create_kid1 of Mom
interface create_kid1()

types

    KID    : address Kid

creates storage KID

    #Kid.parent |-> ACCT_ID

storage

    #Mom.kid1 |-> 0 => KID

if

    #newAddr(ACCT_ID, Nonce_Mom) == KID

iff

    VCallValue == 0
    VCallDepth <Int 1024
```

### create2

```act
behaviour create_kid2 of Mom
interface create_kid2()

types

    KID    : address Kid

creates storage KID

    #Kid.parent |-> ACCT_ID

storage

    #Mom.kid2 |-> 0 => KID

if

    #newAddr(ACCT_ID, 1, Mom_bin_runtime) == KID

iff

    VCallValue == 0
    VCallDepth <Int 1024
```
