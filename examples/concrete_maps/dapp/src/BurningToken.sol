pragma solidity >=0.4.21;

contract BurningToken {
    mapping(address => uint) public balanceOf;
    mapping(address => uint) public approval;
    uint public constant BURN = 10**3;

    function add(uint x, uint y) internal pure returns (uint z) {
        require((z = x + y) >= x);
    }
    function sub(uint x, uint y) internal pure returns (uint z) {
        require((z = x - y) <= x);
    }

    function transfer(address dst, uint value) private {
        balanceOf[msg.sender] = sub(balanceOf[msg.sender], value);
        balanceOf[address(0)] = add(balanceOf[address(0)], BURN);
        balanceOf[dst]        = add(balanceOf[dst], sub(value, BURN));
    }
}
