pragma solidity >=0.5.15;

import {DSTest} from "src/test.sol";
import {Mom} from "src/createContract.sol";

contract MomTest is DSTest {
  Mom mom;

  function setUp() public {
    mom = new Mom();
  }

  function test_create_kid0() public {
    mom.create_kid0();
    assertEq(mom.kid0(), 0xdf4626ABbA225CdC0ED0F4E57707ee0F54322005);
  }

  function test_create_kid1() public {
    mom.create_kid1();
    assertEq(mom.kid1(), 0xdf4626ABbA225CdC0ED0F4E57707ee0F54322005);
  }
}
