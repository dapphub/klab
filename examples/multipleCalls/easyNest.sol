pragma solidity ^0.4.21;
contract Callee {
  uint temperature = 0;
  function tempDelta(uint x) public {
    temperature = temperature + x;
  }
}

contract easyNest {
  Callee callee = new Callee();
  function raiseTemp(uint x) public {
    callee.tempDelta(x);
  }
}
