
-- | Some interesting (corner case) transactions from testnet3, together with their 
-- previous transactions

module Bitcoin.Test.TxCheck.TestNet3 where

--------------------------------------------------------------------------------

import Bitcoin.BlockChain.Tx
import Bitcoin.Protocol

--------------------------------------------------------------------------------

some_testnet3_txs :: [ Tx (Tx RawScript RawScript, RawScript) RawScript ] 
some_testnet3_txs = 

    [ tx_d276a90b      -- note: these ids are the *last* 8 characters of the hash (written in BE as usual), in reverse order 
    , tx_81d78c36      -- (i messed up, but it's not important so i'm leaving it as it is :)
    , tx_e7e62a24
    , tx_5ae6efb0
    , tx_e1e1e1c1
    , tx_21a22580
    , tx_fc2e2bd9
    , tx_fd5a626e
    , tx_6b840973
    , tx_84aeea31
    , tx_d0d37b12
    , tx_a825867d      
    ]

  where

    tx_d276a90b = 
      Tx{_txVersion = 1,
         _txInputs =
           [TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "684d02b7766476fb7e9bfa63f6353dec5fff8f0d5cadadfb05b0768e5dcfb9c5",
                    _txInPrevOutIdx = 1,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "74ea059a63c7ebddaee6805e1560b15c937d99a9ee9745412cbc6d2a0a5f5305",
                                     _txInPrevOutIdx = 0,
                                     _txInScript =
                                       rawScriptFromString
                                         "493046022100c60678f073961429aed11d094f81ea7fdf7ca8fcf9740e9bd492dde43441a9ea022100f78e190f3894f114880c968e60f57f65c93f0ff948c2ebceedb33d6df3e66cff012103abac35cd4dbc714a65b28316527c925302fb2cf2bcdd0c4972c205b443dbd3b2",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 1788650000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a914ef9825e91a283396438a88a88fa10da3d972acd188ac"},
                             TxOutput{_txOutValue = 400000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a914147a957a6a183db5c85c6f3ae3d21f158b66ac0388ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "684d02b7766476fb7e9bfa63f6353dec5fff8f0d5cadadfb05b0768e5dcfb9c5"},
                       rawScriptFromString
                         "493046022100b300f632d6f7109b4888638fdf3ba74e0102b6eabf220bcb4eb212c69a621eb1022100a1ed0a94c9b9253e645441ff96895d18568f2ec2014d35d470d7989b9b47e7c70141061285128c2063e6a670193f51d0050ea8d198f66c272a8c284985a4124a79dccaa92a7d9215c0cdef770a10241af93522848658f7fcfc29977f3462f6885f64e8"),
                    _txInSeqNo = 4294967295}],
         _txOutputs =
           [TxOutput{_txOutValue = 99950000,
                     _txOutScript =
                       rawScriptFromString
                         "76a91421697b2358b62014db97788a995c9341b16d727b88ac"},
            TxOutput{_txOutValue = 300000000,
                     _txOutScript =
                       rawScriptFromString
                         "76a9140fa575195351af1507607824b4817591b9ba8ae988ac"}],
         _txLockTime = LockImmed,
         _txHash =
           hash256FromTextBE
             "cb2710fca03b99502f81d50a40690f160079459f6b1d27aeb13bbaf70ba976d2"}


    tx_81d78c36 = 
      Tx{_txVersion = 1,
         _txInputs =
           [TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "2e131d48f58cbb358cc53967a2fb89a80a6da337cb430fd719f5888af7a48507",
                    _txInPrevOutIdx = 0,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "1d2e52f9916ad806a96d9a7ccd94494e3873a516483acb15a4d1d30623cec601",
                                     _txInPrevOutIdx = 0,
                                     _txInScript =
                                       rawScriptFromString
                                         "483045022100b5ab50db8119ccde0a6fc51837394358869f36bf8d00030f5d3b0153fd1b1cac02201e9bc491960157faf570e85722106e030631df4df2ef510bfa2c09b8e266b21f01210243203d811524ef8f3588e28b4f68b354322d317dca8c77da8a1c11d41a167a31",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 25000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "5121033d091990498104e71aa940771aa2b9946850591e6f4e3eac31d24078cdb20a4121000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f2052ae"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "2e131d48f58cbb358cc53967a2fb89a80a6da337cb430fd719f5888af7a48507"},
                       rawScriptFromString
                         "004830450221009e3787c6d9d6c072093389ecf643975a122e7334b48880fa8c5e5be2473234dd0220041f3f0338f9db2ccde267ef25e7bad7b1e67bca84a837b419f3b501823e0a3b01"),
                    _txInSeqNo = 4294967295}],
         _txOutputs =
           [TxOutput{_txOutValue = 12500000,
                     _txOutScript =
                       rawScriptFromString
                         "76a91417e0a7b539f7479ba80c8f40892ee514f723824388ac"}],
         _txLockTime = LockImmed,
         _txHash =
           hash256FromTextBE
             "bdefb2077a28d419425d8964f7d09eec89b334a258085f14ea60b71a368cd781"}


    tx_e7e62a24 = 
      Tx{_txVersion = 1,
         _txInputs =
           [TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "46f6c120965dec8151704d1a3b00fff5f58285dcc0cadea5bc8841b8b0a5197c",
                    _txInPrevOutIdx = 0,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "2cc4e5994d2244124035f502c6b89770eda8f622d2d7ad7da472e0369521d4d8",
                                     _txInPrevOutIdx = 0,
                                     _txInScript =
                                       rawScriptFromString
                                         "493046022100e17c17e406de953fee109993e982c11df7dd0a74be470f20cda256cbd47f02e9022100a94f19999223bd303127a33afd4b5f21cac608dcef845a2c709caf845364517b012102366665264a482107b204ddc3d0d730e13d838966c3e7675074bfbd6ebefeceff",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 100000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "6352210261411d0de63460bfed73cb871f868bc3064d1db2a09f27b2477852b1811a02ef210261411d0de63460bfed73cb871f868bc3064d1db2a09f27b2477852b1811a02ef52ae67a820080af0b0156c5dd12c820b2b1b4fbfa315d05ac5a0ea2f9a657d4c8881d0869f88a820080af0b0156c5dd12c820b2b1b4fbfa315d05ac5a0ea2f9a657d4c8881d0869f88210261411d0de63460bfed73cb871f868bc3064d1db2a09f27b2477852b1811a02efac68"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "46f6c120965dec8151704d1a3b00fff5f58285dcc0cadea5bc8841b8b0a5197c"},
                       rawScriptFromString
                         "0048304502207d02ce76875b1b3f2b7af9e45954af1ab531da6ab3edd471aa9148f139c8bad1022100f0f85fd987e90a131f2e311acdfe212925e218ffa5cf79e84e9890c2ddbdbd450148304502207d02ce76875b1b3f2b7af9e45954af1ab531da6ab3edd471aa9148f139c8bad1022100f0f85fd987e90a131f2e311acdfe212925e218ffa5cf79e84e9890c2ddbdbd450151"),
                    _txInSeqNo = 4294967295}],
         _txOutputs =
           [TxOutput{_txOutValue = 100000000,
                     _txOutScript =
                       rawScriptFromString
                         "76a91403efb01790d098aef3752449a94a1dc593e527cd88ac"}],
         _txLockTime = LockImmed,
         _txHash =
           hash256FromTextBE
             "4d0bbf6348726a49600171033e456548a09b246829d649e77b929caf242ae6e7"}


    tx_5ae6efb0 = 
      Tx{_txVersion = 1,
         _txInputs =
           [TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "4fed625bfe36c2d17d839a6407be374663ad823c2cde7073319bb51b8025a221",
                    _txInPrevOutIdx = 1,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "c6498e51b170bb66d129a5eab51ead72c6fc7f35f36e78849875d7377a4be8b4",
                                     _txInPrevOutIdx = 4,
                                     _txInScript =
                                       rawScriptFromString
                                         "473044022023c374437ad0416763853830a2ae192f3bc4c49ffc251e0cabfe779ae2c3823c022071bfaa94518780ebf60f456569f8cdf2238577d4eaa1af0144047804778cbc500121032cf0450bb0013ab45b7feb67afe6313e47086b9c5bb13142c46bbb55dbc0d2cd",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 120000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "0130323066643366303435313438356531306633383837363437356630643265396130393739343332353534313766653139316438623963623230653430643863333030326431373463336539306366323433393231383761313037623634373337633937333135633932393264653431373731636565613062323563633534353732653302ae"},
                             TxOutput{_txOutValue = 916990000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9145da359a8f618a3bef736ad51748a63a568b2e87188ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "4fed625bfe36c2d17d839a6407be374663ad823c2cde7073319bb51b8025a221"},
                       rawScriptFromString
                         "47304402200b2f1fd1ffa76514f6fcefd8d80c32a9d0be2b1117127eec831316ecba2ee72e022060b1a6fc82bebbea89d5108724029b956213968c25cbacd1186a83f689465f8c0121024872b6f9eeb8d8255be96d82f8c60885ef401e33a9a4ae680cb5148a8e491899"),
                    _txInSeqNo = 4294967295}],
         _txOutputs =
           [TxOutput{_txOutValue = 896980000,
                     _txOutScript =
                       rawScriptFromString
                         "76a9146ab1d225f182cf669e7b27339ab133e3df93dc0b88ac"},
            TxOutput{_txOutValue = 20000000,
                     _txOutScript =
                       rawScriptFromString
                         "303230666433663034353134383565313066333838373634373566306432653961303937393433323535343137666531393164386239636232306534306438633330ac"}],
         _txLockTime = LockImmed,
         _txHash =
           hash256FromTextBE
             "69f319bc535f576fe2612ae5cae1477c606c3df771bf3f41bc9cfd2db0efe65a"}


    tx_e1e1e1c1 = 
      Tx{_txVersion = 1,
         _txInputs =
           [TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "cc5691f26713fdd9913390fddb2ef0b94b07ceaf7d6ecc8ea7bc20d5d92b2efc",
                    _txInPrevOutIdx = 0,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "a2bcedb144595b9b4d66272dc6927f3efaa251bc1af6f1df5ec599f341737b90",
                                     _txInPrevOutIdx = 0,
                                     _txInScript =
                                       rawScriptFromString
                                         "483045022100dfe55c79d56dfff12319bba3e48b09ba5255d15f9cbce4ea56e058fe820e33a202202cb9b1dcc9da346493aa60fda4170fd985daf920609719e9de647c63a93b5b4701210353b922941fa74bc9d192583041b3cfaadff4698cb1a5e31fbdcd06b63cb9bcdd",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 159980000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9140345ac233f2115dfd3393c408f1ea79d7c5fe22888ac"},
                             TxOutput{_txOutValue = 20000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "00783531213032306664336630343531343835653130663338383736343735663064326539613039373934333235353431376665313931643862396362323065343064386333302130326431373463336539306366323433393231383761313037623634373337633937333135633932393264653431373731636565613062323563633534353732653300783532aeac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "cc5691f26713fdd9913390fddb2ef0b94b07ceaf7d6ecc8ea7bc20d5d92b2efc"},
                       rawScriptFromString
                         "47304402200eb01c33a987bcf9e0c73e3f0325afaddef93a507541866c21125bab795ebb41022007b1d66a25ee16a5ba0033611a196a2532337fb506723bd9166fb545d2b3c1c80121022ef94776731427c07ae880d91206a9fcf3498b374ed9cc1e9f83b092ee02c372"),
                    _txInSeqNo = 4294967295}],
         _txOutputs =
           [TxOutput{_txOutValue = 20000000,
                     _txOutScript =
                       rawScriptFromString
                         "51213032306664336630343531343835653130663338383736343735663064326539613039373934333235353431376665313931643862396362323065343064386333302130326431373463336539306366323433393231383761313037623634373337633937333135633932393264653431373731636565613062323563633534353732653352aeac"},
            TxOutput{_txOutValue = 139970000,
                     _txOutScript =
                       rawScriptFromString
                         "76a914d540dac6f87b99bd9c69c75e2d23553b4946ba4e88ac"}],
         _txLockTime = LockImmed,
         _txHash =
           hash256FromTextBE
             "8e1ae606adfbd418874e151532d2f6a6160fab407f70ef9af2f2d593c1e1e1e1"}


    tx_21a22580 = 
      Tx{_txVersion = 1,
         _txInputs =
           [TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "c6498e51b170bb66d129a5eab51ead72c6fc7f35f36e78849875d7377a4be8b4",
                    _txInPrevOutIdx = 4,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "18951a28f634decb404913045886b7f2c307d8fca7259ac5fa0d468bda9879f7",
                                     _txInPrevOutIdx = 3,
                                     _txInScript =
                                       rawScriptFromString
                                         "47304402205ef3c3153be784f65503a47ef7f82a206a959ba2b468b99462a3b065a691fe710220386e1968ed208749c034583273707bd85e10f85bd06101fabead9a850e2ecdfa0121022a42b63c530a07d1a3c40c0f2f04db32fc344133c7fe433066e0970f8de5fec7",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 41000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a914f1c14050fd77c1e50e2237f2caa985510aa3f43a88ac"},
                             TxOutput{_txOutValue = 45000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a914789fc2b709c4ca027d7dd19b10f9af349d73759288ac"},
                             TxOutput{_txOutValue = 49000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a91496d3ab2cd639e9a2f925bd5b28c4b73676b91a5488ac"},
                             TxOutput{_txOutValue = 55000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a914789fc2b709c4ca027d7dd19b10f9af349d73759288ac"},
                             TxOutput{_txOutValue = 1036990000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a91467ce23788087a8b827828a7a656681f9e5d6cb4188ac"},
                             TxOutput{_txOutValue = 59000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a91496d3ab2cd639e9a2f925bd5b28c4b73676b91a5488ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "c6498e51b170bb66d129a5eab51ead72c6fc7f35f36e78849875d7377a4be8b4"},
                       rawScriptFromString
                         "473044022023c374437ad0416763853830a2ae192f3bc4c49ffc251e0cabfe779ae2c3823c022071bfaa94518780ebf60f456569f8cdf2238577d4eaa1af0144047804778cbc500121032cf0450bb0013ab45b7feb67afe6313e47086b9c5bb13142c46bbb55dbc0d2cd"),
                    _txInSeqNo = 4294967295}],
         _txOutputs =
           [TxOutput{_txOutValue = 120000000,
                     _txOutScript =
                       rawScriptFromString
                         "0130323066643366303435313438356531306633383837363437356630643265396130393739343332353534313766653139316438623963623230653430643863333030326431373463336539306366323433393231383761313037623634373337633937333135633932393264653431373731636565613062323563633534353732653302ae"},
            TxOutput{_txOutValue = 916990000,
                     _txOutScript =
                       rawScriptFromString
                         "76a9145da359a8f618a3bef736ad51748a63a568b2e87188ac"}],
         _txLockTime = LockImmed,
         _txHash =
           hash256FromTextBE
             "4fed625bfe36c2d17d839a6407be374663ad823c2cde7073319bb51b8025a221"}


    tx_fc2e2bd9 = 
      Tx{_txVersion = 1,
         _txInputs =
           [TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "a2bcedb144595b9b4d66272dc6927f3efaa251bc1af6f1df5ec599f341737b90",
                    _txInPrevOutIdx = 0,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "426fcd8e05ceef79a11f0af6f92e3a3415608ae314f1c4491a17190809dc5de1",
                                     _txInPrevOutIdx = 0,
                                     _txInScript =
                                       rawScriptFromString
                                         "483045022100d61262bd05042f074a6f1353a3e6d7791bdb04564598de31f64e6ca54ffd9573022019cec5e57d9e05a46a239632fb5cb8348eb48bd221515af284d972ff22c2a17f01210369e66687279fe6a4495b90b582ea24e2a80832938d796aa613c1f841060beecb",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 179990000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a914f4348d78d3488134665fab624776f9dc2063bafc88ac"},
                             TxOutput{_txOutValue = 20000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "a914c694b8e6c34d5f0e80923e802a828de60a91d9de87"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "a2bcedb144595b9b4d66272dc6927f3efaa251bc1af6f1df5ec599f341737b90"},
                       rawScriptFromString
                         "483045022100dfe55c79d56dfff12319bba3e48b09ba5255d15f9cbce4ea56e058fe820e33a202202cb9b1dcc9da346493aa60fda4170fd985daf920609719e9de647c63a93b5b4701210353b922941fa74bc9d192583041b3cfaadff4698cb1a5e31fbdcd06b63cb9bcdd"),
                    _txInSeqNo = 4294967295}],
         _txOutputs =
           [TxOutput{_txOutValue = 159980000,
                     _txOutScript =
                       rawScriptFromString
                         "76a9140345ac233f2115dfd3393c408f1ea79d7c5fe22888ac"},
            TxOutput{_txOutValue = 20000000,
                     _txOutScript =
                       rawScriptFromString
                         "00783531213032306664336630343531343835653130663338383736343735663064326539613039373934333235353431376665313931643862396362323065343064386333302130326431373463336539306366323433393231383761313037623634373337633937333135633932393264653431373731636565613062323563633534353732653300783532aeac"}],
         _txLockTime = LockImmed,
         _txHash =
           hash256FromTextBE
             "cc5691f26713fdd9913390fddb2ef0b94b07ceaf7d6ecc8ea7bc20d5d92b2efc"}


    tx_fd5a626e = 
      Tx{_txVersion = 1,
         _txInputs =
           [TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "9df6bb44a582c0ab7b5b0bcf2fc8aa35eafe317b0d31dde3275c520f127bd3d0",
                    _txInPrevOutIdx = 0,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "2eac8420136633b93d74d87404e245e5ace80f6cb32024a2710cafabd68844e2",
                                     _txInPrevOutIdx = 0,
                                     _txInScript =
                                       rawScriptFromString
                                         "47304402202f8d23cc52edac158e0ab8d57f0cad8dc2d2d62df67715a2b04e31cc5a32dabb022004dfc14a30b36decaa65da04eab9ee40c70a8eacdbc38f61600d68b072bad90001",
                                     _txInSeqNo = 4294967295},
                             TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "0cdb223332975268f3a3e49461cf87654b355c88aae2d870bcc7ce643156be5f",
                                     _txInPrevOutIdx = 0,
                                     _txInScript =
                                       rawScriptFromString
                                         "47304402203ec301ecb4d2c3ba3844b1e2197813ebb09516953dc9dbe654efb5aa4bbb3fa402201968be259975651609fe7366a14583f1a8acfc8ab918def369986dcb0d6f58f201201b1b01dc829177da4a14551d2fc96a9db00c6501edfa12f22cd9cefd335c227f",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 200000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "210333b572abbbd55e520833043492c496495b8794e0c9f2b50bccb1f7edd8bef8caac7c2103504413c2e4873548c4112988134cd633062e7e634549e3ed50fa4eb7bae81110ac7c5379a820d68df9e32a147cffa36193c6f7c43a1c8c69cda530e1c6db354bfabdcfefaf3c875379a820f531f3041d3136701ea09067c53e7159c8f9b2746a56c3d82966c54bbc553226879a635379825379828779679a68"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "9df6bb44a582c0ab7b5b0bcf2fc8aa35eafe317b0d31dde3275c520f127bd3d0"},
                       rawScriptFromString
                         "20ca42095840735e89283fec298e62ac2ddea9b5f34a8cbb7097ad965b87568100201b1b01dc829177da4a14551d2fc96a9db00c6501edfa12f22cd9cefd335c227f4730440220797afb2e2258ffc9681f353f410086ea331c58522d52bcabb0a8dcf800ab9e7f02202f22577bd839d905b8659d0bf248e81f0824d3974d4e63af22aeed31b36c18bd0100"),
                    _txInSeqNo = 4294967295}],
         _txOutputs =
           [TxOutput{_txOutValue = 100000000,
                     _txOutScript =
                       rawScriptFromString
                         "21036ef62794066933b0790a59275ac5701bcd018b94d19df86a15bbd6df7c272c1dac"}],
         _txLockTime = LockImmed,
         _txHash =
           hash256FromTextBE
             "19c0933970d3e516edfa3c961372b051f65c35eafb1fd7f14b0e1d486e625afd"}


    tx_6b840973 = 
      Tx{_txVersion = 1,
         _txInputs =
           [TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "3d33ab23b1f67e55ade4cddbaec642c3f6b2d8c6bf948e82dd2294b87d8625a8",
                    _txInPrevOutIdx = 0,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "35a2bd68babbea5d87a3c209076195a2c3d3d32aac06035ac90f62a74e9b3dd9",
                                     _txInPrevOutIdx = 0,
                                     _txInScript =
                                       rawScriptFromString
                                         "483045022100a6b83e0b4e21b5abebbf990b9933111927adfab94153a3c569646d934c562d31022042a2cd2842ad2a1263ea520093ceb8b4a90974955eeb9ee6ded97b9f13e9239c01",
                                     _txInSeqNo = 4294967295},
                             TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "204c22154affccff4a5d53e85af0ef6f80788a732004adf0ba3c3c38c9365b4a",
                                     _txInPrevOutIdx = 0,
                                     _txInScript =
                                       rawScriptFromString
                                         "48304502210080b82a63b0c034f51b1841e27715878d26a2cab9edad489622df3ddf4a91409202203ba95f4b7e76b0ee091082864593b67f70ef7ec82645dea8b5a3f85e08453be501201b1b01dc829177da4a14551d2fc96a9db00c6501edfa12f22cd9cefd335c227f",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 19700000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "210333b572abbbd55e520833043492c496495b8794e0c9f2b50bccb1f7edd8bef8caac7c2103504413c2e4873548c4112988134cd633062e7e634549e3ed50fa4eb7bae81110ac7c5379a820d68df9e32a147cffa36193c6f7c43a1c8c69cda530e1c6db354bfabdcfefaf3c875379a820f531f3041d3136701ea09067c53e7159c8f9b2746a56c3d82966c54bbc553226879a635379825379828779679a68"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "3d33ab23b1f67e55ade4cddbaec642c3f6b2d8c6bf948e82dd2294b87d8625a8"},
                       rawScriptFromString
                         "20ca42095840735e89283fec298e62ac2ddea9b5f34a8cbb7097ad965b87568100201b1b01dc829177da4a14551d2fc96a9db00c6501edfa12f22cd9cefd335c227f483045022100849d542d29108ed748f57f6cf072668626755ca7d0150602c2f55cc350aee2d50220354f1ca87561e7b17a47eed221d25712f1038c22f38fcdaae5343d0b6e3bf09e0100"),
                    _txInSeqNo = 4294967295}],
         _txOutputs =
           [TxOutput{_txOutValue = 19600000,
                     _txOutScript =
                       rawScriptFromString
                         "21036ef62794066933b0790a59275ac5701bcd018b94d19df86a15bbd6df7c272c1dac"}],
         _txLockTime = LockImmed,
         _txHash =
           hash256FromTextBE
             "bca9d833f529caa530cf6114d9c57b12e213487416e14091e95c62667309846b"}


    tx_84aeea31 = 
      Tx{_txVersion = 1,
         _txInputs =
           [TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "99b8b9962f25e246eb0a767ed60be7e4d5a9eff74c2479db8727ad2ce4c43c68",
                    _txInPrevOutIdx = 0,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "97b444c615877f7e64c1bb159c241afaacfedf458b08cc0a9ec8e514bbf33f16",
                                     _txInPrevOutIdx = 0,
                                     _txInScript =
                                       rawScriptFromString
                                         "473044022014908a4cf2cac6b8a143f26b26b38c0cb0885cb7b3db6c3e53c4608fd2657f050220388c960657f3bc6f8bdd7db8f4dd7321e72c3b5f05525fe505232624ee1111b801",
                                     _txInSeqNo = 4294967295},
                             TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "639e9e6fd8f1b03ccbe9302f2a74fa523fd2b2ea4cc1d7f94ab8205aae88208e",
                                     _txInPrevOutIdx = 0,
                                     _txInScript =
                                       rawScriptFromString
                                         "483045022100d3c77597af64cb514ec3a3b49c2838922863a910cd7751a8f4dc0b190324c598022011a4155e7e11873dc668c7e9a568f910586f8435d5d69e11273d4555a17a7f1c01201b1b01dc829177da4a14551d2fc96a9db00c6501edfa12f22cd9cefd335c227f",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 199987712,
                                      _txOutScript =
                                        rawScriptFromString
                                          "210333b572abbbd55e520833043492c496495b8794e0c9f2b50bccb1f7edd8bef8caac7c2103504413c2e4873548c4112988134cd633062e7e634549e3ed50fa4eb7bae81110ac7c5379a820d68df9e32a147cffa36193c6f7c43a1c8c69cda530e1c6db354bfabdcfefaf3c875379a820f531f3041d3136701ea09067c53e7159c8f9b2746a56c3d82966c54bbc553226879a5479827701200122a59a5379827701200122a59a6353798277537982778779679a68"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "99b8b9962f25e246eb0a767ed60be7e4d5a9eff74c2479db8727ad2ce4c43c68"},
                       rawScriptFromString
                         "20ca42095840735e89283fec298e62ac2ddea9b5f34a8cbb7097ad965b87568100201b1b01dc829177da4a14551d2fc96a9db00c6501edfa12f22cd9cefd335c227f483045022100d485c55bda525687f66ce22e54f2b37fbe0061e6bf0e405c02f679c920d4783202202cd8fcfd829b9e33dccaa1e2022853e8f34b4db4247833a02a473b40e1d12b3d0100"),
                    _txInSeqNo = 4294967295}],
         _txOutputs =
           [TxOutput{_txOutValue = 199983616,
                     _txOutScript =
                       rawScriptFromString
                         "21036ef62794066933b0790a59275ac5701bcd018b94d19df86a15bbd6df7c272c1dac"}],
         _txLockTime = LockImmed,
         _txHash =
           hash256FromTextBE
             "cdb1f5870375adcbe2ea26fa00b06b5638843cfc00859d63d54c9e3c31eaae84"}


    tx_d0d37b12 = 
      Tx{_txVersion = 1,
         _txInputs =
           [TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "2eac8420136633b93d74d87404e245e5ace80f6cb32024a2710cafabd68844e2",
                    _txInPrevOutIdx = 0,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "376fa876330c1e647f2eb3c9265040e44ab849b32888c4613424210b8fcb7d21",
                                     _txInPrevOutIdx = 1,
                                     _txInScript =
                                       rawScriptFromString
                                         "4730440220111d29b2168e0473b99a8cc1b01a8fffb5fd314ab2127b0a7ab42045ca1b159b02201d4970d6c0bc897110589185e4a8f43c5f2101c72e26bf21ce9c0684425d47d30121036ef62794066933b0790a59275ac5701bcd018b94d19df86a15bbd6df7c272c1d",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 100000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "2103504413c2e4873548c4112988134cd633062e7e634549e3ed50fa4eb7bae81110ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "2eac8420136633b93d74d87404e245e5ace80f6cb32024a2710cafabd68844e2"},
                       rawScriptFromString
                         "47304402202f8d23cc52edac158e0ab8d57f0cad8dc2d2d62df67715a2b04e31cc5a32dabb022004dfc14a30b36decaa65da04eab9ee40c70a8eacdbc38f61600d68b072bad90001"),
                    _txInSeqNo = 4294967295},
            TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "0cdb223332975268f3a3e49461cf87654b355c88aae2d870bcc7ce643156be5f",
                    _txInPrevOutIdx = 0,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "7573354a6facb91ff7510e6d975fd1b1347b35bdf53f3d5b95c56c0370936314",
                                     _txInPrevOutIdx = 0,
                                     _txInScript =
                                       rawScriptFromString
                                         "47304402207a6e5856219564b2c529d8b2ddc1026ce691c924750051c22b79aecd4afea2f002207fea340579adc47fa68193b940b44ba07d7ac62313df49fd38c50b1968043f45012103658f6f842e46eddf36d47a91f285e37f56540878bdb56045275b661832a56c0b",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 100000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "a820f531f3041d3136701ea09067c53e7159c8f9b2746a56c3d82966c54bbc55322688210333b572abbbd55e520833043492c496495b8794e0c9f2b50bccb1f7edd8bef8caac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "0cdb223332975268f3a3e49461cf87654b355c88aae2d870bcc7ce643156be5f"},
                       rawScriptFromString
                         "47304402203ec301ecb4d2c3ba3844b1e2197813ebb09516953dc9dbe654efb5aa4bbb3fa402201968be259975651609fe7366a14583f1a8acfc8ab918def369986dcb0d6f58f201201b1b01dc829177da4a14551d2fc96a9db00c6501edfa12f22cd9cefd335c227f"),
                    _txInSeqNo = 4294967295}],
         _txOutputs =
           [TxOutput{_txOutValue = 200000000,
                     _txOutScript =
                       rawScriptFromString
                         "210333b572abbbd55e520833043492c496495b8794e0c9f2b50bccb1f7edd8bef8caac7c2103504413c2e4873548c4112988134cd633062e7e634549e3ed50fa4eb7bae81110ac7c5379a820d68df9e32a147cffa36193c6f7c43a1c8c69cda530e1c6db354bfabdcfefaf3c875379a820f531f3041d3136701ea09067c53e7159c8f9b2746a56c3d82966c54bbc553226879a635379825379828779679a68"}],
         _txLockTime = LockImmed,
         _txHash =
           hash256FromTextBE
             "9df6bb44a582c0ab7b5b0bcf2fc8aa35eafe317b0d31dde3275c520f127bd3d0"}


    tx_a825867d = 
      Tx{_txVersion = 1,
         _txInputs =
           [TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "35a2bd68babbea5d87a3c209076195a2c3d3d32aac06035ac90f62a74e9b3dd9",
                    _txInPrevOutIdx = 0,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "10b7d0506e41f72baa3801309f4835272968ee5db945cf7c76ed7be8d5796d4a",
                                     _txInPrevOutIdx = 0,
                                     _txInScript =
                                       rawScriptFromString
                                         "483045022100f6f9cd3681346597f5f7f4aa0191e5c93bfc0eb0968c57a2467f844f0393aff60220775385490d4600ed102cb2536bf445dc7580424ce59b4b0a87ae41de6d0c73050121036ef62794066933b0790a59275ac5701bcd018b94d19df86a15bbd6df7c272c1d",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 9900000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "2103504413c2e4873548c4112988134cd633062e7e634549e3ed50fa4eb7bae81110ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "35a2bd68babbea5d87a3c209076195a2c3d3d32aac06035ac90f62a74e9b3dd9"},
                       rawScriptFromString
                         "483045022100a6b83e0b4e21b5abebbf990b9933111927adfab94153a3c569646d934c562d31022042a2cd2842ad2a1263ea520093ceb8b4a90974955eeb9ee6ded97b9f13e9239c01"),
                    _txInSeqNo = 4294967295},
            TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "204c22154affccff4a5d53e85af0ef6f80788a732004adf0ba3c3c38c9365b4a",
                    _txInPrevOutIdx = 0,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "10b7d0506e41f72baa3801309f4835272968ee5db945cf7c76ed7be8d5796d4a",
                                     _txInPrevOutIdx = 1,
                                     _txInScript =
                                       rawScriptFromString
                                         "4730440220398d0c625c26acd9a0b8813aa91df606e1ea3d0b5751e33919383e9c9a9c4b3a0220027ec216c07f087e37b97c2c3fb41ce0f473d51f7f24486477e799c0c833096a012103658f6f842e46eddf36d47a91f285e37f56540878bdb56045275b661832a56c0b",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 9900000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "a820f531f3041d3136701ea09067c53e7159c8f9b2746a56c3d82966c54bbc55322688210333b572abbbd55e520833043492c496495b8794e0c9f2b50bccb1f7edd8bef8caac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "204c22154affccff4a5d53e85af0ef6f80788a732004adf0ba3c3c38c9365b4a"},
                       rawScriptFromString
                         "48304502210080b82a63b0c034f51b1841e27715878d26a2cab9edad489622df3ddf4a91409202203ba95f4b7e76b0ee091082864593b67f70ef7ec82645dea8b5a3f85e08453be501201b1b01dc829177da4a14551d2fc96a9db00c6501edfa12f22cd9cefd335c227f"),
                    _txInSeqNo = 4294967295}],
         _txOutputs =
           [TxOutput{_txOutValue = 19700000,
                     _txOutScript =
                       rawScriptFromString
                         "210333b572abbbd55e520833043492c496495b8794e0c9f2b50bccb1f7edd8bef8caac7c2103504413c2e4873548c4112988134cd633062e7e634549e3ed50fa4eb7bae81110ac7c5379a820d68df9e32a147cffa36193c6f7c43a1c8c69cda530e1c6db354bfabdcfefaf3c875379a820f531f3041d3136701ea09067c53e7159c8f9b2746a56c3d82966c54bbc553226879a635379825379828779679a68"}],
         _txLockTime = LockImmed,
         _txHash =
           hash256FromTextBE
             "3d33ab23b1f67e55ade4cddbaec642c3f6b2d8c6bf948e82dd2294b87d8625a8"}

--------------------------------------------------------------------------------

