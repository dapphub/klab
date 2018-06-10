pragma solidity ^0.4.23;

contract RmulDapp {

    function add(uint x, uint y) internal pure returns (uint z) {
        require((z = x + y) >= x);
    }

    function mul(uint x, uint y) internal pure returns (uint z) {
        require(y == 0 || (z = x * y) / y == x);
    }

    uint constant RAY = 10 ** 27;

    function rmul(uint x, uint y) public pure returns (uint z) {
        z = add(mul(x, y), RAY / 2) / RAY;
    }
}
