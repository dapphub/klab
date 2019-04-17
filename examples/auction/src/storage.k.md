
```k
syntax Int ::= "#Auction.leader"  [function]
rule #Auction.leader => 0

syntax Int ::= "#Auction.price"  [function]
rule #Auction.price => 1

syntax Int ::= "#Auction.clocked"  [function]
rule #Auction.clocked => 2

syntax Int ::= "#Auction.live"  [function]
rule #Auction.live => 3

syntax Int ::= "#Auction.withdrawals" "[" Int "]"  [function]
rule #Auction.withdrawals[A] => #hashedLocation("Solidity", 4, A)
```

