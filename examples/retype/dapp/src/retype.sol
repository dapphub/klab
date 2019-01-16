pragma solidity ^0.4.21;
contract Int {
    function callMe(int y) public pure returns (int) {
      return y;
    }
}
contract Uint {
  Int intt;
    function callIt(uint x) public {
      intt.callMe(int(x));
    }
}
