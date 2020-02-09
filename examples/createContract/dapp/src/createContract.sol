pragma solidity >=0.5.15;

contract Kid {
  address parent;

  constructor() public {
    parent = msg.sender;
  }
}

contract Mom {
  Kid kid0;
  Kid kid1;
  Kid kid2;

  function create_kid0() public {
    kid0 = new Kid();
  }

  function create_kid1() public {
    bytes memory kidBytecode = type(Kid).creationCode;
    assembly {
      sstore(1, create(0, add(kidBytecode, 32), mload(kidBytecode)))
    }
  }

  function create_kid2() public {
    bytes memory kidBytecode = type(Kid).creationCode;
    assembly {
      sstore(2, create2(0, add(kidBytecode, 32), mload(kidBytecode), 1))
    }
  }
}
