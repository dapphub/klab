pragma solidity >=0.5.15;

contract Kid {
  address parent;

  constructor() public {
    parent = msg.sender;
  }
}

contract Mom {
  Kid child;

  function create() public {
    child = new Kid();
  }
}
