pragma solidity >=0.5.15;

contract Kid {
  address public parent;
  string  public constant name = 'Kid V1';
  bytes32 public ID;
  bool    public initialized;

  constructor() public {
      parent = msg.sender;
      uint chainId;
      assembly {
          chainId := chainid
      }
      ID = keccak256(
          abi.encode(
              keccak256('EIP712Domain(string name,string version,uint256 chainId,address verifyingContract)'),
              keccak256(bytes(name)),
              keccak256(bytes('1')),
              chainId,
              address(this)
          )
      );
  }

  function init() external {
    require(msg.sender == parent, 'FORBIDDEN');
    initialized = true;
  }
}

contract Mom {
  address public kid0;
  address public kid1;
  address public kid2;

  function create_kid0() public {
    Kid kid = new Kid();
    kid0 = address(kid);
    kid.init();
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
