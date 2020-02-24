pragma solidity >=0.5.15;

contract Kid {
  address public parent;

  constructor() public {
    parent = msg.sender;
  }
}

contract KidToken {
  address public parent;
  string  public constant name = 'Kid Token V1';
  bytes32 public DOMAIN_SEPARATOR;

  constructor() public {
      parent = msg.sender;
      uint chainId;
      assembly {
          chainId := chainid
      }
      DOMAIN_SEPARATOR = keccak256(
          abi.encode(
              keccak256('EIP712Domain(string name,string version,uint256 chainId,address verifyingContract)'),
              keccak256(bytes(name)),
              keccak256(bytes('1')),
              chainId,
              address(this)
          )
      );
  }
}

contract Mom {
  address public kid0;
  address public kid1;
  address public kid2;
  address public kid3;
  address public kid4;

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

  function create_kid3() public {
    kid3 = address(new KidToken());
  }

  function create_kid4() public {
    bytes memory tokenBytecode = type(KidToken).creationCode;
    assembly {
      sstore(4, create2(0, add(tokenBytecode, 32), mload(tokenBytecode), 1))
    }
  }
}
