requires "../rules.k"

module PROOF-TOKEN_TRANSFER_FAIL
  imports ETHEREUM-SIMULATION
  imports EVM
  imports RULES

// Token_transfer
rule
  <k> #execute ~> CONTINUATION => #halt ~> CONTINUATION </k>
  <exit-code> 1 </exit-code>
  <mode> NORMAL </mode>
  <schedule> BYZANTIUM </schedule>
  <analysis> .Map </analysis>
  <ethereum>
    <evm>
      <output> _ => _ </output>
      <statusCode> _ => EVMC_REVERT </statusCode>
      <callStack> VCallStack </callStack>
      <interimStates> _ </interimStates>
      <touchedAccounts> _ => _ </touchedAccounts>
      <callState>
        <program> #asMapOpCodes(#dasmOpCodes(#parseByteStack("0x608060405260043610610062576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff16806318160ddd1461006757806370a0823114610092578063767b6190146100e9578063a9059cbb14610116575b600080fd5b34801561007357600080fd5b5061007c610163565b6040518082815260200191505060405180910390f35b34801561009e57600080fd5b506100d3600480360381019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190505050610169565b6040518082815260200191505060405180910390f35b3480156100f557600080fd5b5061011460048036038101908080359060200190929190505050610181565b005b34801561012257600080fd5b50610161600480360381019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190803590602001909291905050506101ce565b005b60015481565b60006020528060005260406000206000915090505481565b80600181905550806000803373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000208190555050565b6102166000803373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002054826102e6565b6000803373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020819055506102a06000808473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205482610302565b6000808473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020819055505050565b600081830390508183101515156102fc57600080fd5b92915050565b6000818301905082811015151561031857600080fd5b929150505600a165627a7a723058202b5b950ceb4ead0c6e06ae193219462e93d9606ee490e00db68c17f897d35d250029"), BYZANTIUM)) </program>
        <programBytes> #parseByteStack("0x608060405260043610610062576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff16806318160ddd1461006757806370a0823114610092578063767b6190146100e9578063a9059cbb14610116575b600080fd5b34801561007357600080fd5b5061007c610163565b6040518082815260200191505060405180910390f35b34801561009e57600080fd5b506100d3600480360381019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190505050610169565b6040518082815260200191505060405180910390f35b3480156100f557600080fd5b5061011460048036038101908080359060200190929190505050610181565b005b34801561012257600080fd5b50610161600480360381019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190803590602001909291905050506101ce565b005b60015481565b60006020528060005260406000206000915090505481565b80600181905550806000803373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000208190555050565b6102166000803373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002054826102e6565b6000803373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020819055506102a06000808473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205482610302565b6000808473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020819055505050565b600081830390508183101515156102fc57600080fd5b92915050565b6000818301905082811015151561031857600080fd5b929150505600a165627a7a723058202b5b950ceb4ead0c6e06ae193219462e93d9606ee490e00db68c17f897d35d250029") </programBytes>
        <id> ACCT_ID </id>
        <caller> CALLER_ID </caller>
        <callData> #abiCallData("transfer", #address(ABI_To), #uint(ABI_Value)) </callData>
        <callValue> 0 </callValue>
        <wordStack> .WordStack => _ </wordStack>
        <localMem> .Map => _ </localMem>
        <pc> 0 => _ </pc>
        <gas> VGas => _ </gas>
        <memoryUsed> 0 => _ </memoryUsed>
        <previousGas> _ => _ </previousGas>
        <static> false </static>
        <callDepth> VCallDepth => _ </callDepth>
      </callState>
      <substate>
        <selfDestruct> VSelfDestruct </selfDestruct>
        <log> _ => VLog </log>
        <refund> _ => VRefund </refund>
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
      <activeAccounts> SetItem(ACCT_ID) _ </activeAccounts>
      <accounts>
        <account>
          <acctID> ACCT_ID </acctID>
          <balance> BAL </balance>
          <code> #parseByteStack("0x608060405260043610610062576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff16806318160ddd1461006757806370a0823114610092578063767b6190146100e9578063a9059cbb14610116575b600080fd5b34801561007357600080fd5b5061007c610163565b6040518082815260200191505060405180910390f35b34801561009e57600080fd5b506100d3600480360381019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190505050610169565b6040518082815260200191505060405180910390f35b3480156100f557600080fd5b5061011460048036038101908080359060200190929190505050610181565b005b34801561012257600080fd5b50610161600480360381019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190803590602001909291905050506101ce565b005b60015481565b60006020528060005260406000206000915090505481565b80600181905550806000803373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000208190555050565b6102166000803373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002054826102e6565b6000803373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020819055506102a06000808473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205482610302565b6000808473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020819055505050565b600081830390508183101515156102fc57600080fd5b92915050565b6000818301905082811015151561031857600080fd5b929150505600a165627a7a723058202b5b950ceb4ead0c6e06ae193219462e93d9606ee490e00db68c17f897d35d250029") </code>
          <storage> (_:Map
                      (#hashedLocation("Solidity", 0, ABI_To) |-> (ToBal => _)
                      (#hashedLocation("Solidity", 0, CALLER_ID) |-> (FromBal => _))))
           </storage>
          <nonce> _ </nonce>
        </account>
       ... 
      </accounts>
      <txOrder> _ </txOrder>
      <txPending> _ </txPending>
      <messages> _ </messages>
    </network>
  </ethereum>
requires #rangeAddress(ACCT_ID)
andBool #notPrecompileAddress(ACCT_ID)
andBool #rangeAddress(CALLER_ID)
andBool #rangeAddress(ORIGIN_ID)
andBool #rangeUInt(48, TIME)
andBool #rangeUInt(256, BAL)
andBool VCallDepth <=Int 1024
  andBool #rangeAddress(ABI_To)
  andBool #rangeUInt(256, ABI_Value)
  andBool #rangeUInt(256, FromBal)
  andBool #rangeUInt(256, ToBal)
  andBool VGas >=Int 100000
  andBool notBool (
            (#rangeUInt(256, ToBal +Int ABI_Value)
    andBool (#rangeUInt(256, FromBal -Int ABI_Value)))
  ) 

endmodule
