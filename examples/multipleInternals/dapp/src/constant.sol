pragma solidity >=0.5.6;

contract ConstantTemp {
  uint temperature = 0;
  function tempDelta(uint x) public {
    temperature = add(temperature, x);
    temperature = sub(temperature, x);
  }

  function add(uint x, uint y) internal pure returns (uint z) {
    z = x + y;
    require(z >= x);
  }

  function sub(uint x, uint y) internal pure returns (uint z) {
    z = x - y;
    require(z <= x);
  }
}
