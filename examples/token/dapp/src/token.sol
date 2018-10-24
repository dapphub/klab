contract Token {
  mapping(address => uint) public balanceOf;
  uint public totalSupply;

  function add(uint256 x, uint256 y) internal returns (uint z) {
    z = x + y;
    require(z >= x);
  }

  function sub(uint256 x, uint256 y) internal returns (uint z) {
    z = x - y;
    require(x >= y);
  }
  
  function constructor(uint supply) {
    totalSupply = supply;
    balanceOf[msg.sender] = supply;
  }
  
  function transfer(address to, uint256 value) public {
    balanceOf[msg.sender] = sub(balanceOf[msg.sender], value);
    balanceOf[to] = add(balanceOf[to], value);
  }
}
