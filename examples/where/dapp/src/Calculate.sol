pragma solidity >=0.4.21;

contract Calculate {
    function add(uint x, uint y) internal pure returns (uint z) {
        require((z = x + y) >= x);
    }
    function sub(uint x, uint y) internal pure returns (uint z) {
        require((z = x - y) <= x);
    }
    function mul(uint x, uint y) internal pure returns (uint z) {
        require(y == 0 || (z = x * y) / y == x);
    }

    function run(uint a, uint b) public pure returns (uint e) {
        uint c = sub(a, b);
        uint d = mul(c, b);
        e = add(d, a);
    }
}
