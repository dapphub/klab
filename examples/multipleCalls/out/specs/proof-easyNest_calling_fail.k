requires "../rules.k"

module PROOF-EASYNEST_CALLING_
  imports ETHEREUM-SIMULATION
  imports EVM
  imports RULES

// calling
rule
  <k> #execute => #halt </k>
  <exit-code> 1 </exit-code>
  <mode> NORMAL </mode>
  <schedule> BYZANTIUM </schedule>
  <analysis> .Map </analysis>
  <ethereum>
    <evm>
      <output> _ </output>
      <statusCode> _ => EVMC_REVERT </statusCode>
      <callStack> VCallStack </callStack>
      <interimStates> _ </interimStates>
      <touchedAccounts> _ => _ </touchedAccounts>
      <callState>
        <program> #asMapOpCodes(#dasmOpCodes(#parseByteStack("0x608060405260043610610041576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff168063d98dac6c14610046575b600080fd5b34801561005257600080fd5b5061007160048036038101908080359060200190929190505050610073565b005b6000809054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff166375657f15826040518263ffffffff167c010000000000000000000000000000000000000000000000000000000002815260040180828152602001915050600060405180830381600087803b15801561010357600080fd5b505af1158015610117573d6000803e3d6000fd5b50505050505600a165627a7a72305820b4c968384e01a88397f78b2d71e9efe0264eb65b1d630e10c0a37229bed697a10029"), BYZANTIUM)) </program>
        <programBytes> #parseByteStack("0x608060405260043610610041576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff168063d98dac6c14610046575b600080fd5b34801561005257600080fd5b5061007160048036038101908080359060200190929190505050610073565b005b6000809054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff166375657f15826040518263ffffffff167c010000000000000000000000000000000000000000000000000000000002815260040180828152602001915050600060405180830381600087803b15801561010357600080fd5b505af1158015610117573d6000803e3d6000fd5b50505050505600a165627a7a72305820b4c968384e01a88397f78b2d71e9efe0264eb65b1d630e10c0a37229bed697a10029") </programBytes>
        <id> ACCT_ID </id>
        <caller> CALLER_ID </caller>
        <callData> #abiCallData("raiseTemp", #uint256(ABI_x)) </callData>
        <callValue> 0 </callValue>
        <wordStack> .WordStack => _ </wordStack>
        <localMem> .Map => _ </localMem>
        <pc> 0 => _ </pc>
        <gas> VGas => _ </gas>
        <memoryUsed> 0 => _ </memoryUsed>
        <previousGas> _ => _ </previousGas>
        <static> false </static>
        <callDepth> CALL_DEPTH => _ </callDepth>
      </callState>
      <substate>
        <selfDestruct> VSelfDestruct </selfDestruct>
        <log> VLog </log>
        <refund> VRefund </refund>
      </substate>
      <gasPrice> _ </gasPrice>
      <origin> ORIGIN_ID </origin>
      <previousHash> _ </previousHash>
      <ommersHash> _ </ommersHash>
      <coinbase> _ </coinbase>
      <stateRoot> _ </stateRoot>
      <transactionsRoot> _ </transactionsRoot>
      <receiptsRoot> _ </receiptsRoot>
      <logsBloom> _ </logsBloom>
      <difficulty> _ </difficulty>
      <number> _ </number>
      <gasLimit> _ </gasLimit>
      <gasUsed> _ </gasUsed>
      <timestamp> TIME </timestamp>
      <extraData> _ </extraData>
      <mixHash> _ </mixHash>
      <blockNonce> _ </blockNonce>
      <ommerBlockHeaders> _ </ommerBlockHeaders>
      <blockhash> _ </blockhash>
    </evm>
    <network>
      <activeAccounts> SetItem(ACCT_ID)
      SetItem(CALLEE) </activeAccounts>
      <accounts>
        <account>
          <acctID> CALLEE </acctID>
          <balance> CALLEE_balance </balance>
          <code> #parseByteStack(0x608060405260043610603f576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff16806375657f15146044575b600080fd5b348015604f57600080fd5b50606c60048036038101908080359060200190929190505050606e565b005b8060005401600081905550505600a165627a7a72305820564eb22139837a9c8dd60bc4cc38ed8e7dd77a543af14328fb404a1e8f8340850029) </code>
          <storage> 0 |-> (0 => _ ) </storage>
          <nonce> _ </nonce>
        </account>
        <account>
          <acctID> ACCT_ID </acctID>
          <balance> BAL </balance>
          <code> #parseByteStack(0x608060405260043610610041576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff168063d98dac6c14610046575b600080fd5b34801561005257600080fd5b5061007160048036038101908080359060200190929190505050610073565b005b6000809054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff166375657f15826040518263ffffffff167c010000000000000000000000000000000000000000000000000000000002815260040180828152602001915050600060405180830381600087803b15801561010357600080fd5b505af1158015610117573d6000803e3d6000fd5b50505050505600a165627a7a72305820b4c968384e01a88397f78b2d71e9efe0264eb65b1d630e10c0a37229bed697a10029) </code>
          <storage>   0 |-> (CALLEE) </storage>
          <nonce> _ </nonce>
        </account>
      </accounts>
      <txOrder> _ </txOrder>
      <txPending> _ </txPending>
      <messages> _ </messages>
    </network>
  </ethereum>
requires 0 <=Int ACCT_ID    andBool ACCT_ID    <Int (2 ^Int 160)
andBool 0 <=Int CALLER_ID  andBool CALLER_ID  <Int (2 ^Int 160)
andBool 0 <=Int ORIGIN_ID  andBool ORIGIN_ID  <Int (2 ^Int 160)
  andBool #rangeUInt(256, ABI_x)
  andBool #rangeAddress(CALLEE)
  andBool #rangeUInt(256, CALLEE_balance)
  andBool VGas >Int 300000
  andBool #rangeUInt(256, ABI_x)
  andBool #rangeAddress(CALLEE)
  andBool #rangeUInt(256, CALLEE_balance)
  andBool notBool (
            VGas >Int 300000
  )

endmodule
