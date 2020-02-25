### create

```act
behaviour create_kid0 of Mom
interface create_kid0()

types

    KID : address Kid
    Id  : uint256

creates storage KID

    #Kid.parent  |-> ACCT_ID
    #Kid.ID      |-> Id

storage

    #Mom.kid0 |-> 0 => KID

if

    #newAddr(ACCT_ID, Nonce_Mom) == KID
    Id == keccakIntList(                                                                                                 \
        keccak(#parseByteStackRaw("EIP712Domain(string name,string version,uint256 chainId,address verifyingContract)")) \
        keccak(#parseByteStackRaw("Kid V1"))                                                                             \
        keccak(#parseByteStackRaw("1"))                                                                                  \
        VChainId                                                                                                         \
        KID                                                                                                              \
    )
    VCallDepth < 1024

iff

    VCallValue == 0
```

### create assembly

```act
behaviour create_kid1 of Mom
interface create_kid1()

types

    KID : address Kid
    Id  : uint256

creates storage KID

    #Kid.parent |-> ACCT_ID
    #Kid.ID     |-> Id

storage

    #Mom.kid1 |-> 0 => KID

if

    #newAddr(ACCT_ID, Nonce_Mom) == KID
    Id == keccakIntList(                                                                                                 \
        keccak(#parseByteStackRaw("EIP712Domain(string name,string version,uint256 chainId,address verifyingContract)")) \
        keccak(#parseByteStackRaw("Kid V1"))                                                                             \
        keccak(#parseByteStackRaw("1"))                                                                                  \
        VChainId                                                                                                         \
        KID                                                                                                              \
    )
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
    Id  : uint256

creates storage KID

    #Kid.parent |-> ACCT_ID
    #Kid.ID     |-> Id

storage

    #Mom.kid2 |-> 0 => KID

if

    #newAddr(ACCT_ID, 1, Kid_bin) == KID
    Id == keccakIntList(                                                                                                 \
        keccak(#parseByteStackRaw("EIP712Domain(string name,string version,uint256 chainId,address verifyingContract)")) \
        keccak(#parseByteStackRaw("Kid V1"))                                                                             \
        keccak(#parseByteStackRaw("1"))                                                                                  \
        VChainId                                                                                                         \
        KID                                                                                                              \
    )
    VCallDepth < 1024

iff

    VCallValue == 0
```
