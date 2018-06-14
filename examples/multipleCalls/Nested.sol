pragma solidity ^0.4.21;
contract Callee {
  int temperature = 0;
  function add(int x, int y) internal pure returns (int z) {
    z = x + y;
    // revert iff overflow:
    require(y <= 0 || z > x);
    require(y >= 0 || z < x);
  }

  function tempDelta(int x) public {
    temperature = add(temperature, x);
  }
}

contract Nested {
  Callee callee = new Callee();
  function updateTemp(int x) public {
    callee.tempDelta(x);
  }
}
