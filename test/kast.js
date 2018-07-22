const kast   = require('../lib/kast.js')
const assert = require('assert')

var callData
var flattenedCallData
var callDataDoubled

var wordStack
var wordStackPatternLinear
var wordStackPatternNonLinear

var pattern1
var pattern2

describe('testing KAST format', function() {

  beforeEach(function() {
    callData          = kast.KApply( "<callData>" , [ kast.KApply("_:__EVM-DATA" , [ kast.KInt(10) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KInt(249) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KInt(213) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KInt(224) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(0) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(1) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(2) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(3) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(4) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(5) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(6) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(7) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(8) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(9) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(10) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(11) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(12) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(13) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(14) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(15) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(16) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(17) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(18) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(19) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(20) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(21) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(22) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(23) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(24) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(25) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(26) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(27) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(28) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(29) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(30) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(31) , kast.KInt(32)]) ,
                                                      kast.KApply("_++__EVM-DATA" , [ kast.KApply("#padToWidth" , [kast.KInt(32) , kast.KApply("#asByteStack" , [ kast.KApply("#unsigned" , [kast.KVariable("Delta_D")])])]) , kast.KApply(".WordStack_EVM-DATA" , [])])
                                                      ])])])])])])])])])])])])])])])])])])])])])])])])])])])])])])])])])])]) ] )
                                                    ]
                                   )

    flattenedCallData = kast.KApply( "<callData>" , [ kast.KApply("_:__EVM-DATA" , [ kast.KInt(10) ,
                                                                                     kast.KInt(249) ,
                                                                                     kast.KInt(213) ,
                                                                                     kast.KInt(224) ,
                                                                                     kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(0) , kast.KInt(32)]) ,
                                                                                     kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(1) , kast.KInt(32)]) ,
                                                                                     kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(2) , kast.KInt(32)]) ,
                                                                                     kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(3) , kast.KInt(32)]) ,
                                                                                     kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(4) , kast.KInt(32)]) ,
                                                                                     kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(5) , kast.KInt(32)]) ,
                                                                                     kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(6) , kast.KInt(32)]) ,
                                                                                     kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(7) , kast.KInt(32)]) ,
                                                                                     kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(8) , kast.KInt(32)]) ,
                                                                                     kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(9) , kast.KInt(32)]) ,
                                                                                     kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(10) , kast.KInt(32)]) ,
                                                                                     kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(11) , kast.KInt(32)]) ,
                                                                                     kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(12) , kast.KInt(32)]) ,
                                                                                     kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(13) , kast.KInt(32)]) ,
                                                                                     kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(14) , kast.KInt(32)]) ,
                                                                                     kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(15) , kast.KInt(32)]) ,
                                                                                     kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(16) , kast.KInt(32)]) ,
                                                                                     kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(17) , kast.KInt(32)]) ,
                                                                                     kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(18) , kast.KInt(32)]) ,
                                                                                     kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(19) , kast.KInt(32)]) ,
                                                                                     kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(20) , kast.KInt(32)]) ,
                                                                                     kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(21) , kast.KInt(32)]) ,
                                                                                     kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(22) , kast.KInt(32)]) ,
                                                                                     kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(23) , kast.KInt(32)]) ,
                                                                                     kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(24) , kast.KInt(32)]) ,
                                                                                     kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(25) , kast.KInt(32)]) ,
                                                                                     kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(26) , kast.KInt(32)]) ,
                                                                                     kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(27) , kast.KInt(32)]) ,
                                                                                     kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(28) , kast.KInt(32)]) ,
                                                                                     kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(29) , kast.KInt(32)]) ,
                                                                                     kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(30) , kast.KInt(32)]) ,
                                                                                     kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(31) , kast.KInt(32)]) ,
                                                                                     kast.KApply("_++__EVM-DATA" , [ kast.KApply("#padToWidth" , [kast.KInt(32) , kast.KApply("#asByteStack" , [ kast.KApply("#unsigned" , [kast.KVariable("Delta_D")])])]) , kast.KApply(".WordStack_EVM-DATA" , [])])
                                                                                   ]
                                                                 )
                                                    ]
                                   )

    callDataDoubled   = kast.KApply( "<callData>" , [ kast.KApply("_:__EVM-DATA" , [ kast.KApply("_*_", [ kast.KInt(2) , kast.KInt(10)  ]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("_*_", [ kast.KInt(2) , kast.KInt(249) ]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("_*_", [ kast.KInt(2) , kast.KInt(213) ]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("_*_", [ kast.KInt(2) , kast.KInt(224) ]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(0) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(1) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(2) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(3) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(4) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(5) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(6) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(7) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(8) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(9) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(10) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(11) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(12) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(13) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(14) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(15) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(16) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(17) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(18) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(19) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(20) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(21) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(22) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(23) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(24) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(25) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(26) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(27) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(28) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(29) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(30) , kast.KInt(32)]) ,
                                                      kast.KApply("_:__EVM-DATA" , [ kast.KApply("nthbyteof" , [kast.KVariable("V_u") , kast.KInt(31) , kast.KInt(32)]) ,
                                                      kast.KApply("_++__EVM-DATA" , [ kast.KApply("#padToWidth" , [kast.KInt(32) , kast.KApply("#asByteStack" , [ kast.KApply("#unsigned" , [kast.KVariable("Delta_D")])])]) , kast.KApply(".WordStack_EVM-DATA" , [])])
                                                      ])])])])])])])])])])])])])])])])])])])])])])])])])])])])])])])])])])]) ] )
                                                    ]
                                   )

    wordStack        = kast.KApply("_:__EVM-DATA", [ kast.KInt(1) , kast.KApply("_:__EVM-DATA", [ kast.KInt(3) , kast.KApply(".WordStack_EVM-DATA", []) ]) ])
    wordStackDoubled = kast.KApply("_:__EVM-DATA", [ kast.KApply("_*_", [ kast.KInt(2) , kast.KInt(1) ]) , kast.KApply("_:__EVM-DATA", [ kast.KApply("_*_", [ kast.KInt(2) , kast.KInt(3) ]) , kast.KApply(".WordStack_EVM-DATA", []) ]) ])

    wordStackPatternLinear    = kast.KApply("_:__EVM-DATA", [ kast.KVariable("x") , kast.KApply("_:__EVM-DATA", [ kast.KVariable("x") , kast.KApply(".WordStack_EVM-DATA", []) ]) ])
    wordStackPatternNonLinear = kast.KApply("_:__EVM-DATA", [ kast.KVariable("x") , kast.KApply("_:__EVM-DATA", [ kast.KVariable("y") , kast.KApply(".WordStack_EVM-DATA", []) ]) ])

    pattern1 = v => { return kast.KApply("<callData>", [ v ]) }
    pattern2 = v => { return kast.KApply("_+_", [ kast.KInt(0) , v ]) }
  })

  it('omitArgs should not do anything to tokens', function() {
    assert.deepEqual(kast.omitArgs(kast.KInt(1)), kast.KInt(1))
  })
  it('omitArgs should remove the arguments to the node `<callData>`', function() {
    assert.deepEqual(kast.omitArgs(callData), kast.KApply("<callData>", []))
  })

  it('visitChildren should not do anything to tokens', function() {
    assert.deepEqual(kast.visitChildren(kast.KInt(1), (arg => {})), kast.KInt(1))
  })
  it('visitChildren should be able to replace children with other children', function() {
    assert.deepEqual(kast.visitChildren(callData, (arg => kast.KInt(1))), kast.KApply("<callData>", [kast.KInt(1)]))
  })

  it('visitTopDown should ignore non-KApplys', function() {
    assert.deepEqual(kast.KInt(1), kast.visitTopDown(kast.KInt(1), kast.omitArgs))
  })
  it('visitBottomUp should ignore non-KApplys', function() {
    assert.deepEqual(kast.KInt(1), kast.visitBottomUp(kast.KInt(1), kast.omitArgs))
  })

  it('flattenKLabel of `_:__EVM-DATA`', function() {
    assert.deepEqual(flattenedCallData, kast.visitBottomUp(callData, kast.flattenKLabel("_:__EVM-DATA")))
  })
  it('flattenKLabels of `_:__EVM-DATA`', function() {
    assert.deepEqual(flattenedCallData, kast.flattenKLabels(["_:__EVM-DATA"])(callData))
  })

  it('variables should match anything', function() {
    assert.deepEqual(kast.match(kast.KVariable("test"), kast.KInt(1)), {"test" : kast.KInt(1)})
    assert.deepEqual(kast.match(kast.KVariable("test"), callData), {"test" : callData})
    assert.deepEqual(kast.match(kast.KVariable("test"), flattenedCallData), {"test" : flattenedCallData})
  })

  it('simple nested pattern matches should succeed', function() {
    assert.deepEqual(kast.match(pattern1(kast.KInt(3)), pattern1(kast.KInt(3)))                            , {})
    assert.deepEqual(kast.match(pattern1(kast.KInt(3)), pattern1(kast.KInt(4)))                            , null)
    assert.deepEqual(kast.match(pattern1(kast.KVariable("x")), pattern1(kast.KInt(3)))                     , {"x": kast.KInt(3)})
    assert.deepEqual(kast.match(pattern1(kast.KVariable("x")), pattern1(pattern1(kast.KInt(3))))           , {"x": pattern1(kast.KInt(3))})
    assert.deepEqual(kast.match(pattern1(pattern2(kast.KVariable("x"))), pattern1(pattern2(kast.KInt(3)))) , {"x": kast.KInt(3)})
  })

  it('non-linear matches should behave correctly', function() {
    assert.deepEqual(kast.match(wordStackPatternNonLinear, wordStack) , { "x": kast.KInt(1) , "y": kast.KInt(3) })
    assert.deepEqual(kast.match(wordStackPatternLinear,    wordStack) , null)
  })

  it('substitution should traverse and substitute variables', function() {
    assert.deepEqual(kast.substitute(pattern1(kast.KInt(3)),        {"x": kast.KInt(100)}), pattern1(kast.KInt(3)))
    assert.deepEqual(kast.substitute(pattern1(kast.KVariable("x")), {"x": kast.KInt(100)}), pattern1(kast.KInt(100)))
    assert.deepEqual(kast.substitute(pattern1(kast.KVariable("x")), {"y": kast.KInt(100)}), pattern1(kast.KVariable("x")))

    assert.deepEqual(kast.substitute(kast.KApply("dummy", [pattern1(kast.KVariable("x")) , kast.KVariable("y")]), {"y": kast.KInt(100)                    }), kast.KApply("dummy", [pattern1(kast.KVariable("x")) , kast.KInt(100)]))
    assert.deepEqual(kast.substitute(kast.KApply("dummy", [pattern1(kast.KVariable("x")) , kast.KVariable("y")]), {"y": kast.KInt(100) , "x": kast.KInt(3)}), kast.KApply("dummy", [pattern1(kast.KInt(3)) , kast.KInt(100)]))
    assert.deepEqual(kast.substitute(kast.KApply("dummy", [pattern1(kast.KVariable("x")) , kast.KVariable("x")]), {"y": kast.KInt(100) , "x": kast.KInt(3)}), kast.KApply("dummy", [pattern1(kast.KInt(3)) , kast.KInt(3)]))
  })

  it('rewriting should do what we expect', function() {
    assert.deepEqual(kast.KInt(4), kast.rewriteTopRequire(kast.KInt(3), kast.KInt(4), (subst => true))(kast.KInt(3)))
    assert.deepEqual(kast.KInt(4), kast.rewriteTop(kast.KInt(3), kast.KInt(4))(kast.KInt(3)))
    assert.deepEqual(kast.KInt(4), kast.rewrite(kast.KInt(3), kast.KInt(4))(kast.KInt(3)))

    assert.deepEqual(pattern1(kast.KInt(4)), kast.rewrite(kast.KInt(3), kast.KInt(4))(pattern1(kast.KInt(3))))
    assert.deepEqual(pattern1(kast.KInt(4)), kast.rewrite(pattern1(kast.KVariable("x")), pattern1(kast.KInt(4)))(pattern1(kast.KInt(3))))
  })
})
