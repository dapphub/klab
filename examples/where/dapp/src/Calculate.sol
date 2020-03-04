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

    uint result = 0;
    function run(uint a, uint b) public returns (uint) {
        uint c = sub(a, b);
        uint d = mul(c, b);
        uint e = add(d, a);

        result = e; return e;
    }
}
