requires "../rules.k"

module PROOF-SAFEADD_ADD_
  imports ETHEREUM-SIMULATION
  imports EVM
  imports RULES

// add
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
        <program> #asMapOpCodes(#dasmOpCodes(#parseByteStack("0x608060405260043610603f576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff168063771602f7146044575b600080fd5b348015604f57600080fd5b5060766004803603810190808035906020019092919080359060200190929190505050608c565b6040518082815260200191505060405180910390f35b6000828284019150811015151560a157600080fd5b929150505600a165627a7a723058203b57dad807fdb6480e9790466c6c7c093336994627575d793bb499d56ffb89090029"), BYZANTIUM)) </program>
        <programBytes> #parseByteStack("0x608060405260043610603f576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff168063771602f7146044575b600080fd5b348015604f57600080fd5b5060766004803603810190808035906020019092919080359060200190929190505050608c565b6040518082815260200191505060405180910390f35b6000828284019150811015151560a157600080fd5b929150505600a165627a7a723058203b57dad807fdb6480e9790466c6c7c093336994627575d793bb499d56ffb89090029") </programBytes>
        <id> ACCT_ID </id>
        <caller> CALLER_ID </caller>
        <callData> #abiCallData("add", #uint256(ABI_X), #uint256(ABI_Y)) </callData>
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
      <activeAccounts> SetItem(ACCT_ID) </activeAccounts>
      <accounts>
        <account>
          <acctID> ACCT_ID </acctID>
          <balance> BAL </balance>
          <code> #parseByteStack(0x608060405260043610603f576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff168063771602f7146044575b600080fd5b348015604f57600080fd5b5060766004803603810190808035906020019092919080359060200190929190505050608c565b6040518082815260200191505060405180910390f35b6000828284019150811015151560a157600080fd5b929150505600a165627a7a723058203b57dad807fdb6480e9790466c6c7c093336994627575d793bb499d56ffb89090029) </code>
          <storage> _ </storage>
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
  andBool #rangeUInt(256, ABI_X)
  andBool #rangeUInt(256, ABI_Y)
  andBool #rangeUInt(256, ABI_X +Int ABI_Y)
  andBool VGas >Int 330
  andBool #rangeUInt(256, ABI_X)
  andBool #rangeUInt(256, ABI_Y)
  andBool notBool (
            #rangeUInt(256, ABI_X +Int ABI_Y)
    andBool VGas >Int 330
  )

endmodule
