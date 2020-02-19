```act
behaviour hash0 of EncodePacked
interface hash0(address arg1, address arg2)

for all

    Hash : uint256

iff

    VCallValue == 0

if

  Hash == keccak(#take(20, #asByteStack(arg1 <<Int 96)) ++ #take(20, #asByteStack(arg2 <<Int 96)))

returns Hash
```
