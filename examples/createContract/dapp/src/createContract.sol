pragma solidity >=0.5.15;

contract Kid {
  address public parent;

  constructor() public {
    parent = msg.sender;
  }
}

contract Mom {
  address public kid0;
  address public kid1;
  address public kid2;
  address public kid3;

  function create_kid0() public {
    kid0 = address(new Kid());
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

  function create_kid3(address trait1, address trait2) public {
    bytes memory kidBytecode = type(Kid).creationCode;
    bytes32 salt = keccak256(abi.encodePacked(trait1, trait2));
    assembly {
      sstore(3, create2(0, add(kidBytecode, 32), mload(kidBytecode), salt))
    }
  }
}
