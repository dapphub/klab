
# Accessors

## leader

```act
behaviour leader of Auction
interface leader()

for all

    Leader : address
    
storage

    leader |-> Leader
    
iff

    VCallValue == 0
    
returns Leader
```

## price

```act
behaviour price of Auction
interface price()

for all

    Price : uint256
    
storage

    price |-> Price
    
iff

    VCallValue == 0
    
returns Price
```

## clocked

```act
behaviour clocked of Auction
interface clocked()

for all

    Clocked : uint256
    
storage

    clocked |-> Clocked
    
iff

    VCallValue == 0
    
returns Clocked
```

## live

```act
behaviour live of Auction
interface live()

for all

    Live : uint256
    
storage

    live |-> Live
    
iff

    VCallValue == 0
    
returns Live
```

## withdrawals

```act
behaviour withdrawals of Auction
interface withdrawals(address a)

for all

    Withdrawal : uint256
    
storage

    withdrawals[a] |-> Withdrawal
    
iff

    VCallValue == 0
    
returns Withdrawal
```

# Mutators

## bid

```act
behaviour bid of Auction
interface bid()

for all

    Price      : uint256
    Live       : uint256
    Leader     : address
    Withdrawal : uint256
    Clocked    : uint256

storage

    live                |-> Live
    withdrawals[Leader] |-> Withdrawal  => Withdrawal + Price
    price               |-> Price       => VALUE
    leader              |-> Leader      => CALLER_ID
    clocked             |-> Clocked     => TIME
    
iff

    Live == 1
    VALUE > Price

iff in range uint256

    Withdrawal + Price
```

## withdraw

// behaviour withdraw of Auction
// interface withdraw()
// 
// for all
// 
//     W : uint256
// 
// storage
// 
//     withdrawals[CALLER_ID] |-> W => 0
//
// iff
// 
//     W > 0
//     W <= ACCT_ID_balance
//     VCallValue == 0

## close

```act
behaviour close of Auction
interface close()

for all

    Leader  : address
    Clocked : uint256
    Live    : uint256

storage

    leader  |-> Leader
    clocked |-> Clocked
    live    |-> Live     => 0
    
iff

    CALLER_ID == Leader
    TIME >= Clocked + 64
    VCallValue == 0
    
iff in range uint256

    Clocked + 64
```
