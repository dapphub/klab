### create

```act
behaviour create_kid0 of Mom
interface create_kid0()

types

    KID : address Kid

creates storage KID

    #Kid.parent |-> ACCT_ID

storage

    #Mom.kid0 |-> 0 => KID

if

    #newAddr(ACCT_ID, Nonce_Mom) == KID

iff

    VCallValue == 0
    VCallDepth < 1024
```

### create assembly

```act
behaviour create_kid1 of Mom
interface create_kid1()

types

    KID : address Kid

creates storage KID

    #Kid.parent |-> ACCT_ID

storage

    #Mom.kid1 |-> 0 => KID

if

    #newAddr(ACCT_ID, Nonce_Mom) == KID
    VCallDepth < 1024

iff

    VCallValue == 0
```

### create2

```act
behaviour create_kid2 of Mom
interface create_kid2()

types

    KID : address Kid

creates storage KID

    #Kid.parent |-> ACCT_ID

storage

    #Mom.kid2 |-> 0 => KID

if

    #newAddr(ACCT_ID, 1, Kid_bin) == KID
    VCallDepth < 1024

iff

    VCallValue == 0
```

### create token

```act
behaviour create_kid3 of Mom
interface create_kid3()

types

    TKN              : address KidToken
    Name             : string
    Domain_separator : bytes32

creates storage TKN

    #KidToken.parent           |-> ACCT_ID
    #KidToken.name             |-> Name
    #KidToken.DOMAIN_SEPARATOR |-> Domain_separator

storage

    #Mom.kid3 |-> 0 => TKN

if

    #newAddr(ACCT_ID, Nonce_Mom) == TKN
    VCallDepth < 1024

iff

    VCallValue == 0
```

### create2 token

```act
behaviour create_kid4 of Mom
interface create_kid4()

types

    TKN              : address KidToken
    Name             : string
    Domain_separator : bytes32

creates storage TKN

    #KidToken.parent           |-> ACCT_ID
    #KidToken.name             |-> Name
    #KidToken.DOMAIN_SEPARATOR |-> Domain_separator

storage

    #Mom.kid3 |-> 0 => TKN

if

    #newAddr(ACCT_ID, 1, KidToken_bin) == TKN
    VCallDepth < 1024

iff

    VCallValue == 0
```
