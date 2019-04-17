pragma solidity ^0.5.6;

contract Auction {
    address public leader;
    uint public price;
    uint public clocked;
    uint constant public INTERVAL = 64;
    uint public live;
    mapping (address => uint) public withdrawals;

    constructor() public {
        price = 0;
        leader = msg.sender;
        clocked = now;
        live = 1;
    }

    function bid() public payable {
        require(msg.value > price);
        require(live == 1);
        require(withdrawals[leader] + price >= withdrawals[leader]); // for overflow

        withdrawals[leader] += price;
        price = msg.value;
        leader = msg.sender;
        clocked = now;
    }

    function withdraw() public {
        require(withdrawals[msg.sender] > 0);

        uint owed = withdrawals[msg.sender];
        withdrawals[msg.sender] = 0;
        msg.sender.transfer(owed);
    }

    function close() public {
        require(msg.sender == leader);
        require(now >= clocked + INTERVAL);
        require(clocked + INTERVAL >= clocked); // for overflow

        live = 0;
    }
}
