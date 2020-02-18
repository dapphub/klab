pragma solidity >=0.5.15;

contract EncodePacked {

  function hash0(address arg1, address arg2) public pure returns(bytes32) {
    return keccak256(abi.encodePacked(arg1, arg2));
  }

}
