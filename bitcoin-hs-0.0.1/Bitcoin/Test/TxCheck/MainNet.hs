
-- | Some interesting (corner case) transactions from the main network, together with their 
-- previous transactions

module Bitcoin.Test.TxCheck.MainNet where

--------------------------------------------------------------------------------

import Bitcoin.BlockChain.Tx
import Bitcoin.Protocol

--------------------------------------------------------------------------------

some_mainnet_txs = 

    [ tx_730ee3ae 
    , tx_05f35ee4       -- note: these ids are the *last* 8 characters of the hash (written in BE as usual), in reverse order 
    , tx_4536b1ed       -- (i messed up, but it's not important so i'm leaving it as it is :)
    , tx_c8b2c169
    , tx_cd53d64c
    , tx_1e8c0e2d
    , tx_023ffe8f
    , tx_4d8666fa
    , tx_35078453
    , tx_a2b9f88d
    , tx_687eb861
    , tx_d49ae9c6
    , tx_a0f093d1
    , tx_31a46ded
    , tx_9183e7b3
    , tx_e04baa84
    , tx_604c747a
    , tx_ebfa4562
    , tx_091f8607
    , tx_94bccdc5
    , tx_5d4bfd57
    , tx_bdc6230d
    , tx_e3ba6bf1
    , tx_0b773101
    , tx_ceafe873
    ]

  where

    tx_730ee3ae = 
      Tx{_txVersion = 1,
         _txInputs =
           [TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "406b2b06bcd34d3c8733e6b79f7a394c8a431fbf4ff5ac705c93f4076bb77602",
                    _txInPrevOutIdx = 0,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "4974d995c0291573caa216d538f2d79f27fe4f9fa351314e2c21b554dbdfd87f",
                                     _txInPrevOutIdx = 1,
                                     _txInScript =
                                       rawScriptFromString
                                         "483045022100b6a7fe5eea81894bbdd0df61043e42780543457fa5581ac1af023761a098e92202201d4752785be5f9d1b9f8d362b8cf3b05e298a78c4abff874b838bb500dcf2a120141042e3c4aeac1ffb1c86ce3621afb1ca92773e02badf0d4b1c836eb26bd27d0c2e59ffec3d6ab6b8bbeca81b0990ab5224ebdd73696c4255d1d0c6b3c518a1a053e",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 5000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a914dc44b1164188067c3a32d4780f5996fa14a4f2d988ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "406b2b06bcd34d3c8733e6b79f7a394c8a431fbf4ff5ac705c93f4076bb77602"},
                       rawScriptFromString
                         "493046022100d23459d03ed7e9511a47d13292d3430a04627de6235b6e51a40f9cd386f2abe3022100e7d25b080f0bb8d8d5f878bba7d54ad2fda650ea8d158a33ee3cbd11768191fd004104b0e2c879e4daf7b9ab68350228c159766676a14f5815084ba166432aab46198d4cca98fa3e9981d0a90b2effc514b76279476550ba3663fdcaff94c38420e9d5"),
                    _txInSeqNo = 0}],
         _txOutputs =
           [TxOutput{_txOutValue = 4000000,
                     _txOutScript =
                       rawScriptFromString
                         "76a9149a7b0f3b80c6baaeedce0a0842553800f832ba1f88ac"}],
         _txLockTime = LockImmed,
         _txHash =
           hash256FromTextBE
             "c99c49da4c38af669dea436d3e73780dfdb6c1ecf9958baa52960e8baee30e73"}


    tx_05f35ee4 = 
      Tx{_txVersion = 1,
         _txInputs =
           [TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "7af3b1301126cba1f27c074aa0ecf9ec1337568b46b5c15990fe392fecd6acfd",
                    _txInPrevOutIdx = 0,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "40f35ff0079448b9275c0b532ba839611afbe31eb98ecf406aa1fa8df1ee0103",
                                     _txInPrevOutIdx = 1,
                                     _txInScript =
                                       rawScriptFromString
                                         "483045022100e9bf207bfbd447a17cb6da9d2a6bafb083e6a7a77b7fb493a1a84dc603daafc402207c0518af5bf58fda038ba8b3c588badcfbabd48c3501c599a3a18a922496cae601410437b81272ac842590a9d75b74ddf11faf21e47f1fb42a207ea8a799d42bde1a33f007e6cc6c92a22494a4a0e1a46c71c7fc3ebfedd3e2be03fc27b0e4df69034b",
                                     _txInSeqNo = 4294967295},
                             TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "c99c49da4c38af669dea436d3e73780dfdb6c1ecf9958baa52960e8baee30e73",
                                     _txInPrevOutIdx = 0,
                                     _txInScript =
                                       rawScriptFromString
                                         "483045022100de9eab2250e7cad8349d006d83a436475cc9d53f1b850ec11088524df7b1c9530220115e9f8c1a72f3efecce8551fb5b395fa5f329d915f07626255918731e05e5b2014104d7b6ff1469a6f5da3bc10961b488a0ab6484b800b77c069e4f71ed9b7f698d484845046b0fee8b043fe36dc345a1794f1c26447c45671d5b6012f864ef51a19f",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 5000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9144ad6b326f980e87c3c9e298cce9b773f329d428188ac"},
                             TxOutput{_txOutValue = 3000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a914a691714db17036d30e8b9e8dac9ac0e8e3dc449e88ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "7af3b1301126cba1f27c074aa0ecf9ec1337568b46b5c15990fe392fecd6acfd"},
                       rawScriptFromString
                         "483045022033eb6fc4eee4f925bbe5df26ad8eb3b51ca8e5f599057bcc479aa2d450cb36ed0221009ed0ff46379f89b5f3d2444f715fcd056817d3303c5e3f6318a7d54673fb61ed004104a013b98715379a42aedb498e9b90d054530d01a12e48877267c2bcd169602a38a430ff4134536064f9034ba8fc5731648a8e8f03976e75e07bc114867dbb3446"),
                    _txInSeqNo = 0}],
         _txOutputs =
           [TxOutput{_txOutValue = 4000000,
                     _txOutScript =
                       rawScriptFromString
                         "76a9149a7b0f3b80c6baaeedce0a0842553800f832ba1f88ac"}],
         _txLockTime = LockImmed,
         _txHash =
           hash256FromTextBE
             "0ad07700151caa994c0bc3087ad79821adf071978b34b8b3f0838582e45ef305"}


    tx_4536b1ed = 
      Tx{_txVersion = 1,
         _txInputs =
           [TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "04b63c92e931604c93ac4e09aa9e5cdb6d982c8773c8d55b0da541cb168be961",
                    _txInPrevOutIdx = 1,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "381e0933965cd2dc003d4a266d747805b4b90e01bf7b43afca42810963886259",
                                     _txInPrevOutIdx = 0,
                                     _txInScript =
                                       rawScriptFromString
                                         "4730440220151e5df30ca6b0a2c247cbc9c95a77dbede5954935c3cbd5f6cd91382339430f022059d88d225b09f11459487f61547cd1b4695af55badfdc0de0f5fba7e6a58d5bb014104a9fa929b669d576b25c1c78b1764cc211495655fb8d0591801cf0bb52c0bdacdebac1baeb58c8141b5359caa81d5cb532daf95667898cec3737b38b05c7b74ec",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 32000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9140ae18d248d2857f597c98e3929322f330e0d35c888ac"},
                             TxOutput{_txOutValue = 10000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a914dfe61ae656b348a1de565b04a5c906d6210fe48588ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "04b63c92e931604c93ac4e09aa9e5cdb6d982c8773c8d55b0da541cb168be961"},
                       rawScriptFromString
                         "483045022100b8136d33dcf791c5008b56dbf3ec88ac950d848913a703b6ef577986a3fed027022019b7c1e21d9c8d53257f48b7f0727bf8410525d524c1b594baca0a29311f7c9d0041040c367eea20dfa49a1aac1cef9b26e7e87e029c066bd78e3cf73e3fccbf5ba29987d8486a4cf31bc8297cea1c21d613074a01853811ae3e26ec56e063da51f324"),
                    _txInSeqNo = 0}],
         _txOutputs =
           [TxOutput{_txOutValue = 4000000,
                     _txOutScript =
                       rawScriptFromString
                         "76a9149a7b0f3b80c6baaeedce0a0842553800f832ba1f88ac"},
            TxOutput{_txOutValue = 5000000,
                     _txOutScript =
                       rawScriptFromString
                         "76a9145f5b25b608df561037dfd06df5b4c75641df876f88ac"}],
         _txLockTime = LockImmed,
         _txHash =
           hash256FromTextBE
             "7c451f68e15303ab3e28450405cfa70f2c2cc9fa29e92cb2d8ed6ca6edb13645"}


    tx_c8b2c169 = 
      Tx{_txVersion = 1,
         _txInputs =
           [TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "7c451f68e15303ab3e28450405cfa70f2c2cc9fa29e92cb2d8ed6ca6edb13645",
                    _txInPrevOutIdx = 1,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "04b63c92e931604c93ac4e09aa9e5cdb6d982c8773c8d55b0da541cb168be961",
                                     _txInPrevOutIdx = 1,
                                     _txInScript =
                                       rawScriptFromString
                                         "483045022100b8136d33dcf791c5008b56dbf3ec88ac950d848913a703b6ef577986a3fed027022019b7c1e21d9c8d53257f48b7f0727bf8410525d524c1b594baca0a29311f7c9d0041040c367eea20dfa49a1aac1cef9b26e7e87e029c066bd78e3cf73e3fccbf5ba29987d8486a4cf31bc8297cea1c21d613074a01853811ae3e26ec56e063da51f324",
                                     _txInSeqNo = 0}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 4000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9149a7b0f3b80c6baaeedce0a0842553800f832ba1f88ac"},
                             TxOutput{_txOutValue = 5000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9145f5b25b608df561037dfd06df5b4c75641df876f88ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "7c451f68e15303ab3e28450405cfa70f2c2cc9fa29e92cb2d8ed6ca6edb13645"},
                       rawScriptFromString
                         "4830450221009c3beea64e50cd952532411cac06f0ebedd314eb24869c50274af666ea5ec02b022065c36676885cb708f0706cbc87659a9210507cd6476457ebe44a8a5bbe48a3790041041a94acd54a2bcfc8ce8ec59a07ebc06ea4ec9a994f8927af4850dc6d9f95598aa93186529e0ba6a236c2c6f1c181377e537181f65eb79cb9c9b764c75788ff2e"),
                    _txInSeqNo = 0}],
         _txOutputs =
           [TxOutput{_txOutValue = 1500000,
                     _txOutScript =
                       rawScriptFromString
                         "76a9149a7b0f3b80c6baaeedce0a0842553800f832ba1f88ac"},
            TxOutput{_txOutValue = 2500000,
                     _txOutScript =
                       rawScriptFromString
                         "76a914ba97ff48413d4de71c42ced7d9fec680192f367488ac"}],
         _txLockTime = LockImmed,
         _txHash =
           hash256FromTextBE
             "a6c116351836d9cc223321ba4b38d68c8f0db53661f8c2229acabbc269c1b2c8"}


    tx_cd53d64c = 
      Tx{_txVersion = 1,
         _txInputs =
           [TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "6bd7acdefe329601ff7659138bde836569bd8f415a0a889582c3c14de26a87a1",
                    _txInPrevOutIdx = 0,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "cfd3b57199ed9d19b9b99256320f3be305adad771c85662184968f73c5d9acdf",
                                     _txInPrevOutIdx = 0,
                                     _txInScript =
                                       rawScriptFromString
                                         "483045022100dca97b7e358e1d81a0e4a647bdae3ff6f26aa763f3aa9b3dafa283bdae33a10a02200fd0bd4e7adffca854607d315c0093cd771eeec52bb58afc29171ebeb9e19fb90141046b8c76a1f34604ee741e6ec1deb3215bac12e101d3e24fc73dd6c3b143ae9ac40b93f677268093d06828675a02d43cc3132bd5e6fb3bb80701caba8f3bc8760f",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 100000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9148b30467657a145977bba903d84283b565ac185bf88ac"},
                             TxOutput{_txOutValue = 223000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a914b09207be0402a677fbc7af4e617245850ad9c16388ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "6bd7acdefe329601ff7659138bde836569bd8f415a0a889582c3c14de26a87a1"},
                       rawScriptFromString
                         "48304502206ec57ff846660ffe536d05e10020bb589875f7d1e5eeb5f080308e0f635f84060221009fec54bf52250fe7c999107e1c78f061a78016d51d49b8de04cf92a5de6365b900410458f16d4860d3e01e9a2b57e4e458c8f08ed61a94397aadf0c88e3dd237fbb510cb2760aef26a0c0dd6ba67248992051b2b2e047f0f88450648c2bc4f580921df"),
                    _txInSeqNo = 0}],
         _txOutputs =
           [TxOutput{_txOutValue = 49000000,
                     _txOutScript =
                       rawScriptFromString
                         "76a9149a7b0f3b80c6baaeedce0a0842553800f832ba1f88ac"},
            TxOutput{_txOutValue = 50000000,
                     _txOutScript =
                       rawScriptFromString
                         "76a9143d2008fe12629cabf06e329720407835ab4a3fc788ac"}],
         _txLockTime = LockImmed,
         _txHash =
           hash256FromTextBE
             "f5efee46ccfa4191ccd9d9f645e2f5d09bbe195f95ef5608e992d6794cd653cd"}


    tx_1e8c0e2d = 
      Tx{_txVersion = 1,
         _txInputs =
           [TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "f5efee46ccfa4191ccd9d9f645e2f5d09bbe195f95ef5608e992d6794cd653cd",
                    _txInPrevOutIdx = 1,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "6bd7acdefe329601ff7659138bde836569bd8f415a0a889582c3c14de26a87a1",
                                     _txInPrevOutIdx = 0,
                                     _txInScript =
                                       rawScriptFromString
                                         "48304502206ec57ff846660ffe536d05e10020bb589875f7d1e5eeb5f080308e0f635f84060221009fec54bf52250fe7c999107e1c78f061a78016d51d49b8de04cf92a5de6365b900410458f16d4860d3e01e9a2b57e4e458c8f08ed61a94397aadf0c88e3dd237fbb510cb2760aef26a0c0dd6ba67248992051b2b2e047f0f88450648c2bc4f580921df",
                                     _txInSeqNo = 0}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 49000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9149a7b0f3b80c6baaeedce0a0842553800f832ba1f88ac"},
                             TxOutput{_txOutValue = 50000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9143d2008fe12629cabf06e329720407835ab4a3fc788ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "f5efee46ccfa4191ccd9d9f645e2f5d09bbe195f95ef5608e992d6794cd653cd"},
                       rawScriptFromString
                         "493046022100cf687b15278731ee0ddb392b1bc764d33fc3f2e35fcd8a929a81abec31af1d4b022100a156e6c070726ad6cd2880296ecd440d74cfdc5526c15171b33576c03ca84729004104ad9cec0f378f53270082d595d44da3d9259fdd6b856a000f2e8cf4be754f3bfd848e62ff8edf93204961c9da626b9d12db8d23cffc7ad32b119464fcedb369c5"),
                    _txInSeqNo = 0}],
         _txOutputs =
           [TxOutput{_txOutValue = 24000000,
                     _txOutScript =
                       rawScriptFromString
                         "76a9149a7b0f3b80c6baaeedce0a0842553800f832ba1f88ac"},
            TxOutput{_txOutValue = 25000000,
                     _txOutScript =
                       rawScriptFromString
                         "76a91410a9055904cc30bcbaf5b9e97c1b49d4aa60f08d88ac"}],
         _txLockTime = LockImmed,
         _txHash =
           hash256FromTextBE
             "904bda3a7d3e3b8402793334a75fb1ce5a6ff5cf1c2d3bcbd7bd25872d0e8c1e"}


    tx_023ffe8f = 
      Tx{_txVersion = 1,
         _txInputs =
           [TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "904bda3a7d3e3b8402793334a75fb1ce5a6ff5cf1c2d3bcbd7bd25872d0e8c1e",
                    _txInPrevOutIdx = 1,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "f5efee46ccfa4191ccd9d9f645e2f5d09bbe195f95ef5608e992d6794cd653cd",
                                     _txInPrevOutIdx = 1,
                                     _txInScript =
                                       rawScriptFromString
                                         "493046022100cf687b15278731ee0ddb392b1bc764d33fc3f2e35fcd8a929a81abec31af1d4b022100a156e6c070726ad6cd2880296ecd440d74cfdc5526c15171b33576c03ca84729004104ad9cec0f378f53270082d595d44da3d9259fdd6b856a000f2e8cf4be754f3bfd848e62ff8edf93204961c9da626b9d12db8d23cffc7ad32b119464fcedb369c5",
                                     _txInSeqNo = 0}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 24000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9149a7b0f3b80c6baaeedce0a0842553800f832ba1f88ac"},
                             TxOutput{_txOutValue = 25000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a91410a9055904cc30bcbaf5b9e97c1b49d4aa60f08d88ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "904bda3a7d3e3b8402793334a75fb1ce5a6ff5cf1c2d3bcbd7bd25872d0e8c1e"},
                       rawScriptFromString
                         "48304502200800b74207bd708713893ee143370d78642ff45155d5533f09dc58b48bab08820221008216b942e1b1c44e8c08c1919cc30b06220b28e71228deb809322cb3827a2b02004104a4ead59a9952460a92272e62d6bfcd5551f645212a4b2b00e1181f14fed797a65d06cc8c8f4f25e3920f6be5fd0d6d73144c785939a60fa3cc6697c79cbe484e"),
                    _txInSeqNo = 0}],
         _txOutputs =
           [TxOutput{_txOutValue = 11500000,
                     _txOutScript =
                       rawScriptFromString
                         "76a9149a7b0f3b80c6baaeedce0a0842553800f832ba1f88ac"},
            TxOutput{_txOutValue = 12500000,
                     _txOutScript =
                       rawScriptFromString
                         "76a914abc43ef01ed21912bc6646d2c24136434f69de3588ac"}],
         _txLockTime = LockImmed,
         _txHash =
           hash256FromTextBE
             "8ac76995ce4ac10dd02aa819e7e6535854a2271e44f908570f71bc418ffe3f02"}


    tx_4d8666fa = 
      Tx{_txVersion = 1,
         _txInputs =
           [TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "aa85c2dd8bc29ef4015ed110ba543ea8adbccee2d1f3f51af33fc145c4aa1623",
                    _txInPrevOutIdx = 0,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "d0634be008dcc5920d21c301130e53b157211b07749fb433ecd524d0498c98ba",
                                     _txInPrevOutIdx = 1,
                                     _txInScript =
                                       rawScriptFromString
                                         "48304502200f18c2d1fe6513b90f44513e975e05cc498e7f5a565b46c65b1d448734392c6f022100917766d14f2e9933eb269c83b3ad440ed8432da8beb5733f34046509e48b1d850141049ba39856eec011b79f1acb997760ed9d3f90d477077d17df2571d94b2fa2137bf0976d786b6aabc903746e269628b2c28e4b5db753845e5713a48ee7d6b97aaf",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 3000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9147a2a3b481ca80c4ba7939c54d9278e50189d94f988ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "aa85c2dd8bc29ef4015ed110ba543ea8adbccee2d1f3f51af33fc145c4aa1623"},
                       rawScriptFromString
                         "4b3048022200002b83d59c1d23c08efd82ee0662fec23309c3adbcbd1f0b8695378db4b14e736602220000334a96676e58b1bb01784cb7c556dd8ce1c220171904da22e18fe1e7d1510db5014104d0fe07ff74c9ef5b00fed1104fad43ecf72dbab9e60733e4f56eacf24b20cf3b8cd945bcabcc73ba0158bf9ce769d43e94bd58c5c7e331a188922b3fe9ca1f5a"),
                    _txInSeqNo = 4294967295}],
         _txOutputs =
           [TxOutput{_txOutValue = 3000000,
                     _txOutScript =
                       rawScriptFromString
                         "76a9147a2a3b481ca80c4ba7939c54d9278e50189d94f988ac"}],
         _txLockTime = LockImmed,
         _txHash =
           hash256FromTextBE
             "fb0a1d8d34fa5537e461ac384bac761125e1bfa7fec286fa72511240fa66864d"}


    tx_35078453 = 
      Tx{_txVersion = 1,
         _txInputs =
           [TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "51f2a76c6f86f18079a9fde6e72c5694f392c7829eb9a93f6f6eb4eb9c692e16",
                    _txInPrevOutIdx = 1,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "f6e29be468b308c80d6b64121baa136a96e9ac5d685f644b4bbce2e8eb1ca715",
                                     _txInPrevOutIdx = 49,
                                     _txInScript =
                                       rawScriptFromString
                                         "4930460221008aa83ca31a71a65f429ae46bf216f9cae8ea4474771ec996950821660f4611e8022100e2de287ac2e0edfa90b1363fde96ff0698495eb71e1460ec1e34f56912d3949101410431740cc7170cc597693a91662f4dcf78b3130900cf5f62f0104ae7f3d17856cc0f75780c7cfebd6bcce571fa3c69f2796fd3a7ab7b223750ffed25dabf198e85",
                                     _txInSeqNo = 4294967295},
                             TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "afc5a667734c637512010a647399b061b42d769209e59350651887f9d01c5e2a",
                                     _txInPrevOutIdx = 35,
                                     _txInScript =
                                       rawScriptFromString
                                         "4730440220329aba7a3d19d07d371e001a4d74ee14d69a25b98ac13e5726ae85ce8c90d1c7022035bdb358186091d258fddd7cdf78657d1ddd8adbefd4b245f81d9d34c833475901410431740cc7170cc597693a91662f4dcf78b3130900cf5f62f0104ae7f3d17856cc0f75780c7cfebd6bcce571fa3c69f2796fd3a7ab7b223750ffed25dabf198e85",
                                     _txInSeqNo = 4294967295},
                             TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "fb6ed5c0fd0f4b54f512fe47e153bb5d0644afc192cbafe50753d55ef5f0223f",
                                     _txInPrevOutIdx = 27,
                                     _txInScript =
                                       rawScriptFromString
                                         "483045022100dd6496bf066835511a8d5c0ef640d339dfff51e0263843177e6ecce4db155e89022058f99badfeae1638e463a53e4baac75663482b5c367a9d79fa93cdb9d9349e1a01410431740cc7170cc597693a91662f4dcf78b3130900cf5f62f0104ae7f3d17856cc0f75780c7cfebd6bcce571fa3c69f2796fd3a7ab7b223750ffed25dabf198e85",
                                     _txInSeqNo = 4294967295},
                             TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "475d1cf4d7ab81171e670d6c2c948d15c4a0d80e2aa1eaaea3b512343a5bf757",
                                     _txInPrevOutIdx = 0,
                                     _txInScript =
                                       rawScriptFromString
                                         "473044022037622097853da2b3f2757cb2d3776b5c0f88ec5aba4301c89cd933b02977185b02200c869b080e60b596b0f5540011be681b7b0b2cbead1730beca2653192481b50201410439363df8bfe1566aaa40c5812e9931df6d3cf97455c67dce7c1bf2f74f1564d5646ebae32a435015a68cb73ef14d8c1c8f311a19dbfb461cb161a9e2cfdad64f",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 1002675,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a914ef9b3d09be64c9372c99c6de18ca225e108e63c688ac"},
                             TxOutput{_txOutValue = 50000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9146482d425dc43ff3890071f13c07c42236e50833888ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "51f2a76c6f86f18079a9fde6e72c5694f392c7829eb9a93f6f6eb4eb9c692e16"},
                       rawScriptFromString
                         "493045022100a9379b66c22432585cb2f5e1e85736c69cf5fdc9e1033ad583fc27f0b7c561d802202c7b5d9d92ceca742829ffbe28ba6565faa8f94556cb091cbc39d2f11d45946700014104650a9a1deb523f636379ec70c29b3e1e832e314dea0f791160f3dba628f4f509360e525318bf7892af9ffe2f585bf7b264aa31792744ec1885ce17f3b1ef50f3"),
                    _txInSeqNo = 4294967295}],
         _txOutputs =
           [TxOutput{_txOutValue = 45000000,
                     _txOutScript =
                       rawScriptFromString
                         "76a914ed2533122ffd7f0724c424599206ccb23e89d6f788ac"},
            TxOutput{_txOutValue = 5000000,
                     _txOutScript =
                       rawScriptFromString
                         "76a914c0ac87c28c943caead3e6d64663449bfc9f0a5ce88ac"}],
         _txLockTime = LockImmed,
         _txHash =
           hash256FromTextBE
             "a618dc10b8a1f9d9a1469b3bb84fc17da86df2d51c27e2aa16fa130953840735"}


    tx_a2b9f88d = 
      Tx{_txVersion = 1,
         _txInputs =
           [TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "a618dc10b8a1f9d9a1469b3bb84fc17da86df2d51c27e2aa16fa130953840735",
                    _txInPrevOutIdx = 1,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "51f2a76c6f86f18079a9fde6e72c5694f392c7829eb9a93f6f6eb4eb9c692e16",
                                     _txInPrevOutIdx = 1,
                                     _txInScript =
                                       rawScriptFromString
                                         "493045022100a9379b66c22432585cb2f5e1e85736c69cf5fdc9e1033ad583fc27f0b7c561d802202c7b5d9d92ceca742829ffbe28ba6565faa8f94556cb091cbc39d2f11d45946700014104650a9a1deb523f636379ec70c29b3e1e832e314dea0f791160f3dba628f4f509360e525318bf7892af9ffe2f585bf7b264aa31792744ec1885ce17f3b1ef50f3",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 45000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a914ed2533122ffd7f0724c424599206ccb23e89d6f788ac"},
                             TxOutput{_txOutValue = 5000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a914c0ac87c28c943caead3e6d64663449bfc9f0a5ce88ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "a618dc10b8a1f9d9a1469b3bb84fc17da86df2d51c27e2aa16fa130953840735"},
                       rawScriptFromString
                         "493045022034e4786cf22cd00b45faff8afc3b8789c924378176d934adee0d3b3f4a8bf0dc022100b658dd07beeede1f792d238c3ee29c25200f3b834662f9c900bb4d065526dac900014104b72ce98ffd246b2b52046394e6676b756272693a41c00a4f607bf61130c744b9725508fe33cdacb260ee9175bcfafca49f7544af7f0307a81f4840e6745a51e4"),
                    _txInSeqNo = 4294967295},
            TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "ca4dcdc1d9d64c74593cc7020e0df54e8e5677cb32262112fea828c8d6b9e012",
                    _txInPrevOutIdx = 1,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "3ab74ed863772e882dd1c2a01318ebdcf5346ed862f43f7995591aef70da1b87",
                                     _txInPrevOutIdx = 47,
                                     _txInScript =
                                       rawScriptFromString
                                         "48304502206a5009fb210db932fe138784c788ad8b420d153bdd42e6aa7e54ea03bb39c06e022100fc4034c40cfe985b60e11d59ac5118ce8ac7d6e3e58c259b7635514101b8005c01410431740cc7170cc597693a91662f4dcf78b3130900cf5f62f0104ae7f3d17856cc0f75780c7cfebd6bcce571fa3c69f2796fd3a7ab7b223750ffed25dabf198e85",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 1137896,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9148bd403423a36dd650fb93c499f454ada2e64076b88ac"},
                             TxOutput{_txOutValue = 1000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9146482d425dc43ff3890071f13c07c42236e50833888ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "ca4dcdc1d9d64c74593cc7020e0df54e8e5677cb32262112fea828c8d6b9e012"},
                       rawScriptFromString
                         "493045022052538ceefdadef44696559b5b135e48218403f10120bcf592825b924af804821022100ed30a2a2218ad85438fd6a38f909b5ac55bc322033b63ddf17b3b9db11cd618000014104650a9a1deb523f636379ec70c29b3e1e832e314dea0f791160f3dba628f4f509360e525318bf7892af9ffe2f585bf7b264aa31792744ec1885ce17f3b1ef50f3"),
                    _txInSeqNo = 4294967295}],
         _txOutputs =
           [TxOutput{_txOutValue = 6000000,
                     _txOutScript =
                       rawScriptFromString
                         "76a914ed2533122ffd7f0724c424599206ccb23e89d6f788ac"}],
         _txLockTime = LockImmed,
         _txHash =
           hash256FromTextBE
             "67e758b27df26ad609f943b30e5bbb270d835b737c8b3df1a7944ba08df8b9a2"}


    tx_687eb861 = 
      Tx{_txVersion = 1,
         _txInputs =
           [TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "d7161e20fde5d5c3fd2a050ed5ca4b5099630135e253e8c0347ca2e8516a7967",
                    _txInPrevOutIdx = 1,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "cd6dac2b078a477ec0ced1aac45dfc61cf36abc4dc14bd280b93ce1bb66e764e",
                                     _txInPrevOutIdx = 15,
                                     _txInScript =
                                       rawScriptFromString
                                         "483045022060fe0c18b95e88eadb1329c6e70161a776b5ab7f88a3d5e1cd10f9e2b21d9d76022100bee91d1fc4e09b73c5adece74cd24c6def660b976becd3c167c145d4d372c67a01410431740cc7170cc597693a91662f4dcf78b3130900cf5f62f0104ae7f3d17856cc0f75780c7cfebd6bcce571fa3c69f2796fd3a7ab7b223750ffed25dabf198e85",
                                     _txInSeqNo = 4294967295},
                             TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "83fc7eb1f2557093d49c3422c9af0621f5e36e1ee2c398edf7188d0960a78b46",
                                     _txInPrevOutIdx = 35,
                                     _txInScript =
                                       rawScriptFromString
                                         "48304502204c5cc4f3d25311322db91a98ecf227f4b827b0763810a82be337a7277d767a91022100dd35036a5a1b3391ad03b4ce91ab9020c3e7edfe6f95934a4489d9fd660001df01410431740cc7170cc597693a91662f4dcf78b3130900cf5f62f0104ae7f3d17856cc0f75780c7cfebd6bcce571fa3c69f2796fd3a7ab7b223750ffed25dabf198e85",
                                     _txInSeqNo = 4294967295},
                             TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "2e64b3af2794f18b9dd329ab6273818aebaf991423b7ac8216b73be6e721248d",
                                     _txInPrevOutIdx = 35,
                                     _txInScript =
                                       rawScriptFromString
                                         "47304402203ca0caaf94dabf14d4a9142a769b64de57edadaccd7e896307040e37570bd7d8022056feb48a6ac657bf81f1895da7c503e80a8aa5c0ba6a97aac43403d91f8675a701410431740cc7170cc597693a91662f4dcf78b3130900cf5f62f0104ae7f3d17856cc0f75780c7cfebd6bcce571fa3c69f2796fd3a7ab7b223750ffed25dabf198e85",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 1003890,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9149a51d178508a0129b4f7fabbb9471d33ca761d5288ac"},
                             TxOutput{_txOutValue = 20000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a914d00e78df206bb30c34b4ab01bb775af39def92a188ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "d7161e20fde5d5c3fd2a050ed5ca4b5099630135e253e8c0347ca2e8516a7967"},
                       rawScriptFromString
                         "493045022100eccb0785c18ecd405d37cd0369f7cdadf62c1c5afd8c2fb4a7748c20e3073c3802203f7b1e36de55b38abb4badf71679171c7fada67587dba7f5017f684a840d1c150001410430ed754f2ab8ef5ac46e4622a0e9e79922457fc62e3d2c3c8abccf3fe123ed338fa39be3695ebeafbb6dbd4cb94f81a8b0b738e09bf146af03d4d30135ef3c43"),
                    _txInSeqNo = 4294967295}],
         _txOutputs =
           [TxOutput{_txOutValue = 10000000,
                     _txOutScript =
                       rawScriptFromString
                         "76a914ed2533122ffd7f0724c424599206ccb23e89d6f788ac"},
            TxOutput{_txOutValue = 10000000,
                     _txOutScript =
                       rawScriptFromString
                         "76a9146780e620b8e29804855dbbb6e604f5e38621feed88ac"}],
         _txLockTime = LockImmed,
         _txHash =
           hash256FromTextBE
             "fa253ace4be5a4acde8a6c5e6adc808de965e09f16fc5b65b7cb58f961b87e68"}


    tx_d49ae9c6 = 
      Tx{_txVersion = 1,
         _txInputs =
           [TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "c92e4ac4a54f356d1d2956f952cce054098f0cf61d1e2618b6b090f200ed9ced",
                    _txInPrevOutIdx = 1,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "da475734568820a4c03cfd9e8237a45b47be3500a8869bd16a21933b46a04253",
                                     _txInPrevOutIdx = 0,
                                     _txInScript =
                                       rawScriptFromString
                                         "473044022073efdad5fcf4f63e5529038706baa99d317feebb12225d4f09516eb2faa0940b0220171cadd9b5bf6cfa87c87cf244e1fbd3ca295cb1c658c7ba78ebb07f11e717de0141043712320aaf75db9e016e7777f124c26283e85a58f6ff6a18d6d046bad17e6552355db60bd831bd0721e296b7173da2ab2f16beba09b66b964b593472b91c0a93",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 2741650000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9141991e869480afb2f35696c552546625310d827b288ac"},
                             TxOutput{_txOutValue = 100000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9145811cfd9bc24826c2ed456e14fbd79525387bc9d88ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "c92e4ac4a54f356d1d2956f952cce054098f0cf61d1e2618b6b090f200ed9ced"},
                       rawScriptFromString
                         "49304402206dde40f793d40a241a9d8957f529c82d1988cc53240a0b107741dc06e6753be302203b80dc2ee98c240d6c9bf1b1d03882b8aa8b53e55be480ee557d4c448178222300000141046bb99bb9e71205eb3e993db2274069eba9bac5627e546685aeb01a5acdefc3d704ebafa1a7bff71c8c2b480cc77ee29410d830e3a199546ef7be0a8861b49cf4"),
                    _txInSeqNo = 4294967295}],
         _txOutputs =
           [TxOutput{_txOutValue = 100000000,
                     _txOutScript =
                       rawScriptFromString
                         "76a91407a2bc41a410629bf3055f6c73b0fc32aad822ac88ac"}],
         _txLockTime = LockImmed,
         _txHash =
           hash256FromTextBE
             "bf4a7d685be994bc27ddd82cfd75fe27a8945544ef95e58b62a98c6dc6e99ad4"}


    tx_a0f093d1 = 
      Tx{_txVersion = 1,
         _txInputs =
           [TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "8f86c20eb02bc1d2eabf5d05f04ef3d7464e9e49bc243146188122b1d6eb7858",
                    _txInPrevOutIdx = 1,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "f11475d7de904e6a8238a72bea7b440ef5aad425c2a27225b352eed607f55382",
                                     _txInPrevOutIdx = 0,
                                     _txInScript =
                                       rawScriptFromString
                                         "47304402204ed920cc2cf325366c90ec72c297db1d51f14294a457499f316171fe6e84aba8022034bc817c8b287ad5932e2626bb0d0beb28cf4f875adc4ab6f6fb57fa2f280be201410478fde041341ee1d124dd95aeba0bfc77a4bf8fcf8b51d6c1e749f2e833df2038dc132dca08d6a9c28d77116770afb07fd0a23ffa2a6f6f9f3760ba6a409ffcc0",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 2895550000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9146d771b9503b3417ac2903a81eb97eb07e3e8a61f88ac"},
                             TxOutput{_txOutValue = 203000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9145811cfd9bc24826c2ed456e14fbd79525387bc9d88ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "8f86c20eb02bc1d2eabf5d05f04ef3d7464e9e49bc243146188122b1d6eb7858"},
                       rawScriptFromString
                         "493045022100c8c0430c6266ed3d59932c6616a158af102787162d48fb29e84f20c87a9eeaa802202d71976d98b0eaebde10abd50d7076d9b1f58a80e7c00c3f4cb95efa28834af3000141046bb99bb9e71205eb3e993db2274069eba9bac5627e546685aeb01a5acdefc3d704ebafa1a7bff71c8c2b480cc77ee29410d830e3a199546ef7be0a8861b49cf4"),
                    _txInSeqNo = 4294967295},
            TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "952b3173835462fff02cdf01032246372bc59d6c76563693fc61508a61ddf541",
                    _txInPrevOutIdx = 1,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "1ac1e6b01399fddff78ec578450fb6c6bbefa7cc499fd316c6fcc1a05a40c204",
                                     _txInPrevOutIdx = 0,
                                     _txInScript =
                                       rawScriptFromString
                                         "493046022100b665271efa9844b05fd53ba0c925266f3b1e268814605940eadc1b639a2f8475022100e71e18b7022c48c0b096b0e484ed44c2e21c589925ddb5d4149a00d1752a538c014104f5ba281f1335673f11bcb347d18c85f906b52dd49560d7851b722fb8cb290a4ffa8c46a459299b09f4c0c39d36312910f6ed70be2592ba3f6f0a2cfb6c52a253",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 4473900000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a914a5f77f4cc8e084054f0b202c3ccf8ced2b63aa7c88ac"},
                             TxOutput{_txOutValue = 202000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9145811cfd9bc24826c2ed456e14fbd79525387bc9d88ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "952b3173835462fff02cdf01032246372bc59d6c76563693fc61508a61ddf541"},
                       rawScriptFromString
                         "493044022007733199509695d8d6cbf81401ce8256f2a90f298a8bd1b57f98fbb3562a86bd0220412f4d9aec3644a36922050ff28f5371b4a49542f6a214ba50beca9b33f2468e00000141046bb99bb9e71205eb3e993db2274069eba9bac5627e546685aeb01a5acdefc3d704ebafa1a7bff71c8c2b480cc77ee29410d830e3a199546ef7be0a8861b49cf4"),
                    _txInSeqNo = 4294967295},
            TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "5f097c7f98f4e7c30ba45ac8558f2e0312734b20c3ba8eebed3c7a16f0cbb08d",
                    _txInPrevOutIdx = 1,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "73c4c02476ef6865a02f6e805611f8346fb3e33d24e79e402172dd4c091cdbec",
                                     _txInPrevOutIdx = 0,
                                     _txInScript =
                                       rawScriptFromString
                                         "473044022046822d74067f96a45ad9c882a020db0cde815c2464d4a3dc9ca99ba7ca5a37cb02201a90455d5da20cec3718fe2a4ae3323a5d70a89897aa31a2987a656a5cbdb85d014104de0117a204f91892e96170d38b2683f76a6892a3ea7878dce5604e55603eb36161d01e5046733866b763d9832ab2b2dde6ddd999af07b22d5bb365b9508991a3",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 2571100000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a914f5e205e30e51a668fbb4d5fc4bda29addce525b588ac"},
                             TxOutput{_txOutValue = 204000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9145811cfd9bc24826c2ed456e14fbd79525387bc9d88ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "5f097c7f98f4e7c30ba45ac8558f2e0312734b20c3ba8eebed3c7a16f0cbb08d"},
                       rawScriptFromString
                         "49304402205cb326a8520a4bd15457052cb8958c3210f504db102b4df11ac214956066158d02202300c578f2c87ceac66b20ac1ab263d4c3da0cd721e4361c7454153788ec66f100000141046bb99bb9e71205eb3e993db2274069eba9bac5627e546685aeb01a5acdefc3d704ebafa1a7bff71c8c2b480cc77ee29410d830e3a199546ef7be0a8861b49cf4"),
                    _txInSeqNo = 4294967295}],
         _txOutputs =
           [TxOutput{_txOutValue = 609000000,
                     _txOutScript =
                       rawScriptFromString
                         "76a91407a2bc41a410629bf3055f6c73b0fc32aad822ac88ac"}],
         _txLockTime = LockImmed,
         _txHash =
           hash256FromTextBE
             "1514d862dac15a975ca8f24d0fe91c1207df6bcef84a33fbf34d61bdd193f0a0"}


    tx_31a46ded = 
      Tx{_txVersion = 1,
         _txInputs =
           [TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "0e7a6d3b1db50702f878c7a4f7aee599055809dee3bf4fcae9dd7441d8e29742",
                    _txInPrevOutIdx = 0,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "0b468c6431c4fd5f20947e8377d1c2ff51e1b7a03e304d4a4ac038a2e066a8b4",
                                     _txInPrevOutIdx = 1,
                                     _txInScript =
                                       rawScriptFromString
                                         "47304402204d93087c767244e82ec398dc450c9636c512d99c2f3a33314331a605374a29d502204b5eeae6322f14d38840cf259042e84796dd53386a1a4c0f714c6c27df8b1434014104ad842f4c519ab01398842706a255d91d707e55ec367b388ed6dcbffea6f4221b327150e5797f62ada089bbbda2f1d4ceeccf84af810d158d6a829054827f8830",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 20000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9145811cfd9bc24826c2ed456e14fbd79525387bc9d88ac"},
                             TxOutput{_txOutValue = 30000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a914034cc996b2274be3869ea34b3367db543cef396888ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "0e7a6d3b1db50702f878c7a4f7aee599055809dee3bf4fcae9dd7441d8e29742"},
                       rawScriptFromString
                         "4930450220500b771d7750d8c27009f4e5544f16298cf9b8d9398634428d0b28712ed08206022100a57a1a51b5bdd72a867b4c59a4b8d53270965a543b0c815a1ba73dcbe4a7c427000141046bb99bb9e71205eb3e993db2274069eba9bac5627e546685aeb01a5acdefc3d704ebafa1a7bff71c8c2b480cc77ee29410d830e3a199546ef7be0a8861b49cf4"),
                    _txInSeqNo = 4294967295}],
         _txOutputs =
           [TxOutput{_txOutValue = 20000000,
                     _txOutScript =
                       rawScriptFromString
                         "76a9145811cfd9bc24826c2ed456e14fbd79525387bc9d88ac"}],
         _txLockTime = LockImmed,
         _txHash =
           hash256FromTextBE
             "5d998482db63386ed1d5e5162341cf4c4d66600530b4bb6bb379ae2ded6da431"}


    tx_9183e7b3 = 
      Tx{_txVersion = 1,
         _txInputs =
           [TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "fa253ace4be5a4acde8a6c5e6adc808de965e09f16fc5b65b7cb58f961b87e68",
                    _txInPrevOutIdx = 1,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "d7161e20fde5d5c3fd2a050ed5ca4b5099630135e253e8c0347ca2e8516a7967",
                                     _txInPrevOutIdx = 1,
                                     _txInScript =
                                       rawScriptFromString
                                         "493045022100eccb0785c18ecd405d37cd0369f7cdadf62c1c5afd8c2fb4a7748c20e3073c3802203f7b1e36de55b38abb4badf71679171c7fada67587dba7f5017f684a840d1c150001410430ed754f2ab8ef5ac46e4622a0e9e79922457fc62e3d2c3c8abccf3fe123ed338fa39be3695ebeafbb6dbd4cb94f81a8b0b738e09bf146af03d4d30135ef3c43",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 10000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a914ed2533122ffd7f0724c424599206ccb23e89d6f788ac"},
                             TxOutput{_txOutValue = 10000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9146780e620b8e29804855dbbb6e604f5e38621feed88ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "fa253ace4be5a4acde8a6c5e6adc808de965e09f16fc5b65b7cb58f961b87e68"},
                       rawScriptFromString
                         "493044022069d10669c2b9a44ee6c2fd65472bcf1c5289139d86d78f0a7e89aee1a2f708bd02206f75efe6b265c4557f4531eec6d3a5605eb94361bf3613539d2ecfcdcb260f20000001410406fd69f31ae65d787abe3fcbc1984092b67f14e39c11b98f08fb95a59b4ef9d1f738b7daac59dd72e53b878a29dafee5f4d8b29ee1f903e8703f61577d4f2478"),
                    _txInSeqNo = 4294967295},
            TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "107b6d106c172eec43c3e00d9b67149a7166acc66bcf554a94d035f3011ae033",
                    _txInPrevOutIdx = 1,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "953560a343fa2148d5ec119c82aa3a602b757188f2325a988991c2d16b7b8c3d",
                                     _txInPrevOutIdx = 42,
                                     _txInScript =
                                       rawScriptFromString
                                         "493046022100898505820e878f9decc73a5ce680ec21a02638166e09a07319a410e51814151c0221008d6ac3f693e1e403fcf5633eca32bbd879ab98f02e5bee1f4ee08cafdd7b1ff101410431740cc7170cc597693a91662f4dcf78b3130900cf5f62f0104ae7f3d17856cc0f75780c7cfebd6bcce571fa3c69f2796fd3a7ab7b223750ffed25dabf198e85",
                                     _txInSeqNo = 4294967295},
                             TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "50f5bb0cd700f7f84cc21b91da8c176e58254e1c68127632998ada5d3b770bad",
                                     _txInPrevOutIdx = 22,
                                     _txInScript =
                                       rawScriptFromString
                                         "48304502210097db607d138210576c9a1ce64921b1ca22a245c69f45aed2e6a1682f7f4eb3ef022009a7a8c07487a4689d6ddea101fc1e015ba647b80eee9f30b6fb1c2498b009f101410431740cc7170cc597693a91662f4dcf78b3130900cf5f62f0104ae7f3d17856cc0f75780c7cfebd6bcce571fa3c69f2796fd3a7ab7b223750ffed25dabf198e85",
                                     _txInSeqNo = 4294967295},
                             TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "f186bda534dfebd9f6c30b48651a30d89b16fe650981824ecd4719ff076609c7",
                                     _txInPrevOutIdx = 42,
                                     _txInScript =
                                       rawScriptFromString
                                         "4830450221009b1d1b2a4436b1a133678e05e60ee6be757949d5434b41e1bdf7efed11605cfe02207e07db43fdcbdc703b701dad0a30d54b1e791e4e80edfaa38f310072ada6521101410431740cc7170cc597693a91662f4dcf78b3130900cf5f62f0104ae7f3d17856cc0f75780c7cfebd6bcce571fa3c69f2796fd3a7ab7b223750ffed25dabf198e85",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 1005004,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a914d86f419df0ffa113e03f50ea4863b7fd29f1bf6d88ac"},
                             TxOutput{_txOutValue = 30000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9146482d425dc43ff3890071f13c07c42236e50833888ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "107b6d106c172eec43c3e00d9b67149a7166acc66bcf554a94d035f3011ae033"},
                       rawScriptFromString
                         "49304402201884e4345c43f844fec8223f48d0ec469f00361a5108f9b80d67e34cee01c179022047afec73d1a46b81d9156566dcd4808c2646c0367cf094e8d0a61cbab5bb5bc10000014104650a9a1deb523f636379ec70c29b3e1e832e314dea0f791160f3dba628f4f509360e525318bf7892af9ffe2f585bf7b264aa31792744ec1885ce17f3b1ef50f3"),
                    _txInSeqNo = 4294967295}],
         _txOutputs =
           [TxOutput{_txOutValue = 40000000,
                     _txOutScript =
                       rawScriptFromString
                         "76a914ed2533122ffd7f0724c424599206ccb23e89d6f788ac"}],
         _txLockTime = LockImmed,
         _txHash =
           hash256FromTextBE
             "f7caea2ca2497a0834e6796ca153a231fe4fcfac5655d3f24e7cdafbb3e78391"}


    tx_e04baa84 = 
      Tx{_txVersion = 1,
         _txInputs =
           [TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "142ecc76b7eb80deadecf613bd6d0c6769ce7a037a4cadfc5c6e85a8ad285b2b",
                    _txInPrevOutIdx = 1,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "a618dc10b8a1f9d9a1469b3bb84fc17da86df2d51c27e2aa16fa130953840735",
                                     _txInPrevOutIdx = 0,
                                     _txInScript =
                                       rawScriptFromString
                                         "483045022100819bf0ac422d50a53f08059883a2d7c973d2c8bc5437d647fb0653891d68140902205f14e0bbebd9d59266ec0bc045ae55dd4005b2f10c85fa11d506a2eaf08a63b1014104d45e87999dbda5813f4a49f1771089e0734934076edd56a80b5db776f9adb26a8b04e9fe96ff4b68c83ff73ee5593673ccb2ac66cdca839fc3ede154ca60f007",
                                     _txInSeqNo = 4294967295},
                             TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "67e758b27df26ad609f943b30e5bbb270d835b737c8b3df1a7944ba08df8b9a2",
                                     _txInPrevOutIdx = 0,
                                     _txInScript =
                                       rawScriptFromString
                                         "493046022100f9796d57e3bc3b1f9dfbcf3e9fbae693b870256148e80ee2ff45fc073b872b27022100ef98653cc48a873aa60c1bc59a3d162c304185786043360ce905d86c9e350f00014104d45e87999dbda5813f4a49f1771089e0734934076edd56a80b5db776f9adb26a8b04e9fe96ff4b68c83ff73ee5593673ccb2ac66cdca839fc3ede154ca60f007",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 1000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9149eb05b74b8dddc42616aaea22f394c811f2b03ae88ac"},
                             TxOutput{_txOutValue = 50000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9146780e620b8e29804855dbbb6e604f5e38621feed88ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "142ecc76b7eb80deadecf613bd6d0c6769ce7a037a4cadfc5c6e85a8ad285b2b"},
                       rawScriptFromString
                         "49304402200c0d2dc9c18728ebbd3241dc2b1c96fee8811a92bea50baf677cfb8862022407022056ea02f73c0bc3933ae916df568d7fee33b5a3a38e7cf088c36654c158895bd8000001410406fd69f31ae65d787abe3fcbc1984092b67f14e39c11b98f08fb95a59b4ef9d1f738b7daac59dd72e53b878a29dafee5f4d8b29ee1f903e8703f61577d4f2478"),
                    _txInSeqNo = 4294967295}],
         _txOutputs =
           [TxOutput{_txOutValue = 29950000,
                     _txOutScript =
                       rawScriptFromString
                         "76a914ed2533122ffd7f0724c424599206ccb23e89d6f788ac"},
            TxOutput{_txOutValue = 20000000,
                     _txOutScript =
                       rawScriptFromString
                         "76a914366d23e8a4d8eec194fc5aed4809baea16d1cf1288ac"}],
         _txLockTime = LockImmed,
         _txHash =
           hash256FromTextBE
             "44eaf9d33e0afff32102bd9c68e4cbe510ae894135c253fd7ab1b31b84aa4be0"}


    tx_604c747a = 
      Tx{_txVersion = 1,
         _txInputs =
           [TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "44eaf9d33e0afff32102bd9c68e4cbe510ae894135c253fd7ab1b31b84aa4be0",
                    _txInPrevOutIdx = 1,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "142ecc76b7eb80deadecf613bd6d0c6769ce7a037a4cadfc5c6e85a8ad285b2b",
                                     _txInPrevOutIdx = 1,
                                     _txInScript =
                                       rawScriptFromString
                                         "49304402200c0d2dc9c18728ebbd3241dc2b1c96fee8811a92bea50baf677cfb8862022407022056ea02f73c0bc3933ae916df568d7fee33b5a3a38e7cf088c36654c158895bd8000001410406fd69f31ae65d787abe3fcbc1984092b67f14e39c11b98f08fb95a59b4ef9d1f738b7daac59dd72e53b878a29dafee5f4d8b29ee1f903e8703f61577d4f2478",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 29950000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a914ed2533122ffd7f0724c424599206ccb23e89d6f788ac"},
                             TxOutput{_txOutValue = 20000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a914366d23e8a4d8eec194fc5aed4809baea16d1cf1288ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "44eaf9d33e0afff32102bd9c68e4cbe510ae894135c253fd7ab1b31b84aa4be0"},
                       rawScriptFromString
                         "493045022075a7269ad5506a755ca2ccb586fbf4bcf4177546f0ba58aa65051a8efe840b43022100d99c68e841e9a495ada20d0c7f870b3388d724d3be5b0ada5768d75f62179fc600014104487e33b0220a1c5e07ee58eea30b495af1a3fcc792426be1d438a3fcb2b15975e2daba3ab003f92a341a0ce8f9f8631100ad518900deeeeda218d6214573ef70"),
                    _txInSeqNo = 4294967295}],
         _txOutputs =
           [TxOutput{_txOutValue = 20000000,
                     _txOutScript =
                       rawScriptFromString
                         "76a914ed2533122ffd7f0724c424599206ccb23e89d6f788ac"}],
         _txLockTime = LockImmed,
         _txHash =
           hash256FromTextBE
             "5d2bea10e0c490d9d2a22fd06afe0bbde9cb7992ce71795deb3ea55d7a744c60"}


    tx_ebfa4562 = 
      Tx{_txVersion = 1,
         _txInputs =
           [TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "ef6320d8d6526ce725dcc571e663a3fa0c56892e57b81596d255c427e978a5a9",
                    _txInPrevOutIdx = 1,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "243764be1475ed8a835d97c4d2c432e963641a03888955ec7fe1dbd513a718c7",
                                     _txInPrevOutIdx = 0,
                                     _txInScript =
                                       rawScriptFromString
                                         "493046022100ff5423db95f904d56ed6ed3c24bf702827d1ca37ce0eab434a86cc8e9755912402210080d493ee6f68869ce40f817a61df40423dd330eeb2dd369dbeae7b9f9d99b2de0141046858c3c7650e08d46f176ef7b4f6349b96170297bdba628df212940652f9205523548e82dc334d9114bbd8d8c1573aefc3c375b26fd41ce0b0b3e088b6fefc34",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 1966600000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a91486d3a2725708a306599b7882192bd7e5b948864588ac"},
                             TxOutput{_txOutValue = 111000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9145811cfd9bc24826c2ed456e14fbd79525387bc9d88ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "ef6320d8d6526ce725dcc571e663a3fa0c56892e57b81596d255c427e978a5a9"},
                       rawScriptFromString
                         "4930450220592094e2a695e11d15f5df67e904b4d3ae0ab47ffa683f47523639ea87fc8f6a022100becbb1e41e308c772485bc1dc9ab636872775130f500a2ceda4e342745334512000141046bb99bb9e71205eb3e993db2274069eba9bac5627e546685aeb01a5acdefc3d704ebafa1a7bff71c8c2b480cc77ee29410d830e3a199546ef7be0a8861b49cf4"),
                    _txInSeqNo = 4294967295},
            TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "f0d87a5e3aeb9323a77734ef681041ec790d728ac7a2f2e2b47cd66c0d57d2eb",
                    _txInPrevOutIdx = 1,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "9b780a438631841f6f531ade459325f074a950d2512be1c42bbeebdbcd4d736c",
                                     _txInPrevOutIdx = 0,
                                     _txInScript =
                                       rawScriptFromString
                                         "49304602210083f579b1014becd31b6e46d5bf5a9deebef8b03e9d905610f4bff305a285c60e0221009cf42cb4cd01da8c5571023365a20db512d709ed65bf07a7b8e4b4914a80040901410442134b142182118036b5ad0e92cf22adafb77b7b58a69f164509ee8d5649c6ce74953a6c8dbe8c41d362a31e1d2dc0a0b22206c8b2ebd88fc1868c21a1752483",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 4679000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a914509211cc2aa109e14e935682e4365a75b6af10c288ac"},
                             TxOutput{_txOutValue = 102000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9145811cfd9bc24826c2ed456e14fbd79525387bc9d88ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "f0d87a5e3aeb9323a77734ef681041ec790d728ac7a2f2e2b47cd66c0d57d2eb"},
                       rawScriptFromString
                         "49304502210090e7ccf541d80ff2391770b0e55e7e8e7f1bf93391465d3a5452241e4bc7c32102207154d3b9c9acd3833a5fb73e2139398cd4961411148aae4681031570e3ddbf34000141046bb99bb9e71205eb3e993db2274069eba9bac5627e546685aeb01a5acdefc3d704ebafa1a7bff71c8c2b480cc77ee29410d830e3a199546ef7be0a8861b49cf4"),
                    _txInSeqNo = 4294967295},
            TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "1d88d2da685244eaeb15a9f2fee7f138e707c9ec3d3a5e6b4aa1e18782d3246f",
                    _txInPrevOutIdx = 1,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "7b4b954187d67cfa3ababdc9290fc6532ff6ba746c936ed09951583f8a2e1498",
                                     _txInPrevOutIdx = 0,
                                     _txInScript =
                                       rawScriptFromString
                                         "48304502210090f343095f72989c67a2805d39ac588a59d260a19a331203f27c9828e2a94a6802202bec6f44d079fa13844a4bdef7c7ba433fb7992f6089c5e6d4a2e663527a3562014104320f45acdf7fdff17db0e7bd7a1d60a74800a39123f787759f5f13f1d5d80961a957e8635cf183dca06e7caff95fb212f13dd3a461c15fc2fb7f3702692b1b91",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 829000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a914959994a5dd96e8106bf95b52b8f3fd1b659f406f88ac"},
                             TxOutput{_txOutValue = 100000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9145811cfd9bc24826c2ed456e14fbd79525387bc9d88ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "1d88d2da685244eaeb15a9f2fee7f138e707c9ec3d3a5e6b4aa1e18782d3246f"},
                       rawScriptFromString
                         "49304502207228a087f90ba0837d4ec772c3ec75d16af0dd83c43a7a69300a2c451b8e46b1022100dfb13618538ae3c175d75196afd58b267eb34fbb992f040727027aaa04444c97000141046bb99bb9e71205eb3e993db2274069eba9bac5627e546685aeb01a5acdefc3d704ebafa1a7bff71c8c2b480cc77ee29410d830e3a199546ef7be0a8861b49cf4"),
                    _txInSeqNo = 4294967295},
            TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "b8e2762a53932a03f4391fb8dbad8b6c569b3e6906cb0b1388fae467d3a62576",
                    _txInPrevOutIdx = 1,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "c8f683cd83f9588411d6be8e5dcd76b73b098ccaf91de07806c0a36a5218ef76",
                                     _txInPrevOutIdx = 0,
                                     _txInScript =
                                       rawScriptFromString
                                         "483045022100ecb2befc7136fe7136c8560ee5169e80e70ecf0e41708cd7fbca81f342c81dd002206b082be9cf05b10a48bed8de0b7a7ec143dc96268bae05c170f1a7050f9a82ac01410486813aa6043da060efb73832d98842c05dcde860ea261b5736698d4b3ffb105dd5a161b10aa42ed80ea8c5ffd17f2e51c9d5701f5ee0d889efcfd93ee3806f20",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 4476000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a914fcf176ad6b1bc51d84952186f5c7999ac17f3c1d88ac"},
                             TxOutput{_txOutValue = 103000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9145811cfd9bc24826c2ed456e14fbd79525387bc9d88ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "b8e2762a53932a03f4391fb8dbad8b6c569b3e6906cb0b1388fae467d3a62576"},
                       rawScriptFromString
                         "493045022042a32d7a600f34c8d50adee2949eee6945ca84ebb3c7382d5917c4786869194e022100fa96b92d1d02354f3874bf30c0419378c2f0e2b12149ea00a8af2e55e87efd27000141046bb99bb9e71205eb3e993db2274069eba9bac5627e546685aeb01a5acdefc3d704ebafa1a7bff71c8c2b480cc77ee29410d830e3a199546ef7be0a8861b49cf4"),
                    _txInSeqNo = 4294967295},
            TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "5d998482db63386ed1d5e5162341cf4c4d66600530b4bb6bb379ae2ded6da431",
                    _txInPrevOutIdx = 0,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "0e7a6d3b1db50702f878c7a4f7aee599055809dee3bf4fcae9dd7441d8e29742",
                                     _txInPrevOutIdx = 0,
                                     _txInScript =
                                       rawScriptFromString
                                         "4930450220500b771d7750d8c27009f4e5544f16298cf9b8d9398634428d0b28712ed08206022100a57a1a51b5bdd72a867b4c59a4b8d53270965a543b0c815a1ba73dcbe4a7c427000141046bb99bb9e71205eb3e993db2274069eba9bac5627e546685aeb01a5acdefc3d704ebafa1a7bff71c8c2b480cc77ee29410d830e3a199546ef7be0a8861b49cf4",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 20000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9145811cfd9bc24826c2ed456e14fbd79525387bc9d88ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "5d998482db63386ed1d5e5162341cf4c4d66600530b4bb6bb379ae2ded6da431"},
                       rawScriptFromString
                         "493044022072fcdbdda8f114cc9a2f3dacea580c5bbd7d9c0d0a9f0f11fe03055e79632f0f02205889578897993e42a9322514c34903838e9a45d39776a38178d263b8b84d38fe00000141046bb99bb9e71205eb3e993db2274069eba9bac5627e546685aeb01a5acdefc3d704ebafa1a7bff71c8c2b480cc77ee29410d830e3a199546ef7be0a8861b49cf4"),
                    _txInSeqNo = 4294967295},
            TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "fc807e3afe8537c14985564b43daa7c91cc1c7763718e1fb6ec97f445889ecf2",
                    _txInPrevOutIdx = 1,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "193057432eb62013cf5340a4c9840c442dc387b944825a8f4ca9e6634204a87b",
                                     _txInPrevOutIdx = 0,
                                     _txInScript =
                                       rawScriptFromString
                                         "483045022018a93b89749c1a585f1bba44bbac07baab067e06e8e85f2b6c74cf58b6c0dfca02210091ab2f613993566a47892e42aeb3b72ab0837a015538c4a731d59ed5a6c1e5a9014104dc0e0fb8fe0abed31501181387cbc0a5077742b83f3daf9b5d50108737b77ededa9af2e96a6d8af8bb34e9614743138f1b499e05059dc58792fb852cca9fd917",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 1459700000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a914da011bd8a24d8f05bc03b2eab13dafcabea7094788ac"},
                             TxOutput{_txOutValue = 101000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9145811cfd9bc24826c2ed456e14fbd79525387bc9d88ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "fc807e3afe8537c14985564b43daa7c91cc1c7763718e1fb6ec97f445889ecf2"},
                       rawScriptFromString
                         "493045022100b654d9f6b4dd6f61efc2b5cb60317163edd657c4c3271ffbfa70c4114d7e037202207d2898b8b6de69a8dddd16ce273c0a6b838a98b62d1131bf0c65b26876a7f651000141046bb99bb9e71205eb3e993db2274069eba9bac5627e546685aeb01a5acdefc3d704ebafa1a7bff71c8c2b480cc77ee29410d830e3a199546ef7be0a8861b49cf4"),
                    _txInSeqNo = 4294967295}],
         _txOutputs =
           [TxOutput{_txOutValue = 537000000,
                     _txOutScript =
                       rawScriptFromString
                         "76a91407a2bc41a410629bf3055f6c73b0fc32aad822ac88ac"}],
         _txLockTime = LockImmed,
         _txHash =
           hash256FromTextBE
             "b0da453243e44774165c58e2c0dc015523a0b026e8de6a0268c414526245faeb"}


    tx_091f8607 = 
      Tx{_txVersion = 1,
         _txInputs =
           [TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "e1fa331402e0300103a3ef146c36c8a335690276119cd875e0d19aeefdb8fdd2",
                    _txInPrevOutIdx = 1,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "b53634d338243b9594edafdb2eef5b6bbf7f962e23a3a80b595dae5b563fe193",
                                     _txInPrevOutIdx = 0,
                                     _txInScript =
                                       rawScriptFromString
                                         "48304502206f084949a9799d0745a360512688125aed79d8e1fe92edc4365712b18b07fd5a022100addbddcb9d7dddfa1a43587d2e626412687ad15423171c55b08920658e0c77f901410401f9e29ee22afdc9da35cca2f8c6dc2872bafe6574e7264265f2bd479c76e96116e3faa136115322bf90863460b3437618566a82683df4a17f5ab5b006f0c767",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 4680000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a914141be5d84d2dc5224c17f4abc1072f3e26e18f6988ac"},
                             TxOutput{_txOutValue = 107000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9145811cfd9bc24826c2ed456e14fbd79525387bc9d88ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "e1fa331402e0300103a3ef146c36c8a335690276119cd875e0d19aeefdb8fdd2"},
                       rawScriptFromString
                         "493046022100ef93d10c4e96163294b1f7a0fa6aa43e663ae0ef67fc7a28bb013990f7e67a82022100f33c6819559ad2798b84f532e10c9e6cd4c4f48429dc770b0b68ff437e31a99b0141046bb99bb9e71205eb3e993db2274069eba9bac5627e546685aeb01a5acdefc3d704ebafa1a7bff71c8c2b480cc77ee29410d830e3a199546ef7be0a8861b49cf4"),
                    _txInSeqNo = 4294967295},
            TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "b9d980377586cd6ea08f77c8a4a271e7c8e13b14ba491c716c6e9e99b6a4efea",
                    _txInPrevOutIdx = 1,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "6d7b8e19fa99be1297eb51c076ac74b6799ac0db1f2a7f21e9e204ae271aaf9f",
                                     _txInPrevOutIdx = 0,
                                     _txInScript =
                                       rawScriptFromString
                                         "483045022027854976957927f55ea4b5ccf3e7c067451a8ca1f3f4a7c5303d185393a421a5022100a152a811fd72fceccb211d6c615836baf76e7478b5e95d9221dedaad504ca8a4014104d15a93cd6b9f85342e37a9ef97d9b9f014fb7b31d535501d0dea9f472332e62bc892dd267ed11517ba02f3766a2f2f4594c2454d425d8f5c11d95ecd32a3cc2b",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 3528400000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a914172d73f7280673284d0c792e08d133c635a4796188ac"},
                             TxOutput{_txOutValue = 105000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9145811cfd9bc24826c2ed456e14fbd79525387bc9d88ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "b9d980377586cd6ea08f77c8a4a271e7c8e13b14ba491c716c6e9e99b6a4efea"},
                       rawScriptFromString
                         "493045022057a44f21fd2d0ef10fd61c6d55ed00bbd219a92e93d3b836b9a47d2233d68c3f022100cbe4e7d43f1b9b88faeb0a4c54f7e99132f316ea6ddb5bbf6bd54c67145c3c34000141046bb99bb9e71205eb3e993db2274069eba9bac5627e546685aeb01a5acdefc3d704ebafa1a7bff71c8c2b480cc77ee29410d830e3a199546ef7be0a8861b49cf4"),
                    _txInSeqNo = 4294967295},
            TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "3817470b9dda6b351bdfd359fbb942a44bac813d3700f6131f95aff79847c950",
                    _txInPrevOutIdx = 1,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "0bd1a86d5bde99cdfe76688ef429a332e6570baaf823fcc21c90c573df7cede4",
                                     _txInPrevOutIdx = 1,
                                     _txInScript =
                                       rawScriptFromString
                                         "48304502200153c83886cb49498d8f492783ab6c8a64454cf58e92bb9f01601cc534a194ae022100d43ee30c43de297b473a4aec9beeacb703abc62e68ca86ded67f57640f1ed271014104a4ca069784ba620949dd951a73334902532b44ff39114a4bf167f2a35ff41ad4550835caaabe1eabd2db51e13bfd46cee79bb663a0d4dd23cad01f25f2613615",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 4793950000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9144c952d87fdacb3efd71a77fff0b5bffbae55b41d88ac"},
                             TxOutput{_txOutValue = 106000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9145811cfd9bc24826c2ed456e14fbd79525387bc9d88ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "3817470b9dda6b351bdfd359fbb942a44bac813d3700f6131f95aff79847c950"},
                       rawScriptFromString
                         "493045022031b043ed0f2ea3527559f894a0194c441437ff64194c3241c4701c3d5728651a022100a936f61e5edc299161d219f17dbd81633a71f6915028e283c9d4c01203522e52000141046bb99bb9e71205eb3e993db2274069eba9bac5627e546685aeb01a5acdefc3d704ebafa1a7bff71c8c2b480cc77ee29410d830e3a199546ef7be0a8861b49cf4"),
                    _txInSeqNo = 4294967295}],
         _txOutputs =
           [TxOutput{_txOutValue = 318000000,
                     _txOutScript =
                       rawScriptFromString
                         "76a91407a2bc41a410629bf3055f6c73b0fc32aad822ac88ac"}],
         _txLockTime = LockImmed,
         _txHash =
           hash256FromTextBE
             "9e6b3f44fd66736c9cf21f74a5aaf83f2a7a55157754e90a7b98367107861f09"}


    tx_94bccdc5 = 
      Tx{_txVersion = 1,
         _txInputs =
           [TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "93410afe4caa118778129c7c47ff9dc62aa3fe0ba780730558ed933d3dcc3f73",
                    _txInPrevOutIdx = 1,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "124170dc17331e3b91727ff6d12d6477fe0aa27892b4860c30fa06916e7ac464",
                                     _txInPrevOutIdx = 1,
                                     _txInScript =
                                       rawScriptFromString
                                         "47304402207caaa9f8b0c54320e18848ec0b38881b34820fca2e11e001c9e1521a3980928c0220296a40d49f2f8b6efd67560966ac9464c42f2d86aaeda7df278907c2ed732526014104822b31052b011b375ecfc9c02ad7775c271cde778cb2c159af4e8cbbee9879375012502f4aecd651105e7b1f54512cfd74efb5fde154a422e4a53ea6a710cfab",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 17391000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a91409999cd6449662dfc0445f66274eca425637049b88ac"},
                             TxOutput{_txOutValue = 800000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9143094a64bfbd6b4d4b00616e8ddea4357cef046e888ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "93410afe4caa118778129c7c47ff9dc62aa3fe0ba780730558ed933d3dcc3f73"},
                       rawScriptFromString
                         "4c7b304502210099d6f5897eec6f2c4aeb3ccb43dc19f45f4a43372fd68a9e835bec463159e6620220365d554d87d656907af6a9c98768900c7e8cfbd3352d108c48d383cd6b08f6a02a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a014104bfe59667e7c6cae6dd44d8ed646c49a38186189cf4a84cbb6b563fa914d1549b4d1cad016dba65b06e154e1e6470af96d461931430355f6c0632ac8da5d5f026"),
                    _txInSeqNo = 4294967295}],
         _txOutputs =
           [TxOutput{_txOutValue = 30000000,
                     _txOutScript =
                       rawScriptFromString
                         "76a914f40b83ea6101795594e18532354cd57f824bb08388ac"},
            TxOutput{_txOutValue = 770000000,
                     _txOutScript =
                       rawScriptFromString
                         "76a914f5be45a3b4988c4722a4a4086b44b436d640562388ac"}],
         _txLockTime = LockImmed,
         _txHash =
           hash256FromTextBE
             "23befff6eea3dded0e34574af65c266c9398e7d7d9d07022bf1cd526c5cdbc94"}


    tx_5d4bfd57 = 
      Tx{_txVersion = 1,
         _txInputs =
           [TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "7ee8db476fb0a606342894be732371d27c30e5fcf608648d0a3bfdfe2e971e2c",
                    _txInPrevOutIdx = 1,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "c82611d8caf383e3fafd22a94cea365a3a39d8649502f7385b48f38faf5457bc",
                                     _txInPrevOutIdx = 0,
                                     _txInScript =
                                       rawScriptFromString
                                         "47304402206b467bbb7913ded004a76fbc8a7939f54acaac906e14e5d2f1a32e13b1acd5c50220238102e9f8a5b5e80370cdcdf30d790fa1cdca7e0e44ea15e3bcc0ff717c52aa014104f5cbd2a15a18a16de7b58f7948188f0a58b0272ee8ac94c9c2fbe2b40a1df9619fda8cfc2b756860b1180fc581d25454546d7cb36363bd57fbabd3d42451e4fb",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 1639100000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a91441f45bc676c0e895e461ad91925e83ed5040210988ac"},
                             TxOutput{_txOutValue = 108000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9145811cfd9bc24826c2ed456e14fbd79525387bc9d88ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "7ee8db476fb0a606342894be732371d27c30e5fcf608648d0a3bfdfe2e971e2c"},
                       rawScriptFromString
                         "493045022100ed30336f7416e0631e4e7c80944dcebf0ece2916f42289d81ced9aa7a017a70602202f4c72f1f3dc749b3b04395b43328cb96d45f44ac78d2aea31e37b3772f61cfd000141046bb99bb9e71205eb3e993db2274069eba9bac5627e546685aeb01a5acdefc3d704ebafa1a7bff71c8c2b480cc77ee29410d830e3a199546ef7be0a8861b49cf4"),
                    _txInSeqNo = 4294967295},
            TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "6b9579352579b10b17c1e3ccc7be80220592decd0c6f4da7d6611ac572643d57",
                    _txInPrevOutIdx = 1,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "fa5aaa915b7bf1cf7e5024bbb14c4af6d0c06bed16736113c31802bced802076",
                                     _txInPrevOutIdx = 0,
                                     _txInScript =
                                       rawScriptFromString
                                         "48304502206267d09d04f9810f6a94d1cc3de6179889fe47473b4f307256edfd0664886dd7022100a02826e3ba893e9ca9a9b1a47cb75470219da5b65ff6cb32d653b9219b87728301410464edf34ced124b2d7bac27662cb49e29fbbccb47cdfc250d0c0774dce4937d5b0921f106e7ed63513be7844edf17ec0181ae490fd014b136601add707924726a",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 1974950000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a914f92d40956981e4968afc9d9cd755b117edd3392a88ac"},
                             TxOutput{_txOutValue = 101000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9145811cfd9bc24826c2ed456e14fbd79525387bc9d88ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "6b9579352579b10b17c1e3ccc7be80220592decd0c6f4da7d6611ac572643d57"},
                       rawScriptFromString
                         "493045022100d6413e381fde6a61c7c11d9dd36dcb14e5d14409301b70ba7e6b71d8e767f22c022041ba41e2a03f0f69760490de1d4ab7812f509dce16c642516b8c8bb1c00cc932000141046bb99bb9e71205eb3e993db2274069eba9bac5627e546685aeb01a5acdefc3d704ebafa1a7bff71c8c2b480cc77ee29410d830e3a199546ef7be0a8861b49cf4"),
                    _txInSeqNo = 4294967295}],
         _txOutputs =
           [TxOutput{_txOutValue = 209000000,
                     _txOutScript =
                       rawScriptFromString
                         "76a91407a2bc41a410629bf3055f6c73b0fc32aad822ac88ac"}],
         _txLockTime = LockImmed,
         _txHash =
           hash256FromTextBE
             "cdbfc6cd3b9a06c1d0449cecb90beaa03aaf88e55c3f150924c5d81f57fd4b5d"}


    tx_bdc6230d = 
      Tx{_txVersion = 1,
         _txInputs =
           [TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "17240584a732a9e5f52a9bdabd2564117c4af36b659fda4def73a049db4213d7",
                    _txInPrevOutIdx = 1,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "8511749eb5b231b1a001a03424e6ecd4abb56116c4a4d220e1fd863d696495e6",
                                     _txInPrevOutIdx = 0,
                                     _txInScript =
                                       rawScriptFromString
                                         "493046022100a8ef023dc3bde77a2e129c7b6995bccf61406f0921e2a900f2404a66f702c726022100cc749a817a1fd4f2a34ab183fdc5808aa291d4ad601b55f9e8103ef27b45fbc9014104f83d42fbc7ef731d3fb71edfbb7873460213eca09e1bec98d913df447790b98f6974efdcc2b0b6fb89967023fac45dea4f9356f56d8c972d796b7755555e3ed6",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 2221350000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9147252727b63061be190a224a2f6cd9313930ee88c88ac"},
                             TxOutput{_txOutValue = 103000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9145811cfd9bc24826c2ed456e14fbd79525387bc9d88ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "17240584a732a9e5f52a9bdabd2564117c4af36b659fda4def73a049db4213d7"},
                       rawScriptFromString
                         "493045022100a6b39421a5af610aa67e5b6864ac755c91f4e68009c58edd285248329f0d3f510220332bfe7ea273dcffa28282dac06ddaf9eda17fefd5b699738f3204b75254414a000141046bb99bb9e71205eb3e993db2274069eba9bac5627e546685aeb01a5acdefc3d704ebafa1a7bff71c8c2b480cc77ee29410d830e3a199546ef7be0a8861b49cf4"),
                    _txInSeqNo = 4294967295}],
         _txOutputs =
           [TxOutput{_txOutValue = 103000000,
                     _txOutScript =
                       rawScriptFromString
                         "76a91407a2bc41a410629bf3055f6c73b0fc32aad822ac88ac"}],
         _txLockTime = LockImmed,
         _txHash =
           hash256FromTextBE
             "2b42ba68645d659b4868f32b9785c69115c5d06bacef26fb504e7dc40d23c6bd"}


    tx_e3ba6bf1 = 
      Tx{_txVersion = 1,
         _txInputs =
           [TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "917446e8d16aeb1402ea42b1f6af1a0c295447df3a4d8b1cc3245d4b23fa0554",
                    _txInPrevOutIdx = 1,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "d1e9fab307959a7c91f8a3008602da21e4e00a077577e34a7a81b6987ae79f1c",
                                     _txInPrevOutIdx = 0,
                                     _txInScript =
                                       rawScriptFromString
                                         "48304502201f08c0b6bed12d359ae77ce6ffff3ea6bf661d51352b0ac1c30e9dda1409ea4e022100d6810ae25f3d63fa63487655bcf84bd916eaf783fc5f45cb0c321132920f13ae0141047812733c010f4f3ff41aaa20fffad25c085f1139e45ec7d29a0f0398262afd325a419911c209f4fafcc4afa58b81e4dd75aceff307ed19980e5d75eeb2a9a309",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 1948500000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a914a691b8089a7829f934a847f11a5137c91e8ef53c88ac"},
                             TxOutput{_txOutValue = 121000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9145811cfd9bc24826c2ed456e14fbd79525387bc9d88ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "917446e8d16aeb1402ea42b1f6af1a0c295447df3a4d8b1cc3245d4b23fa0554"},
                       rawScriptFromString
                         "49304402201dc5f9692b472b3383f86ef4dd7db6a3577ad58dee57104c35172ac0cedd46b302200bd6fb71ce91bfa9f626bf71324a6007c8bebc92433fee2fe48e8938b1c6909600000141046bb99bb9e71205eb3e993db2274069eba9bac5627e546685aeb01a5acdefc3d704ebafa1a7bff71c8c2b480cc77ee29410d830e3a199546ef7be0a8861b49cf4"),
                    _txInSeqNo = 4294967295},
            TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "48f1ac1936de25270d1e3e35e2edf849509fe973c52c88447a92d479eac6979c",
                    _txInPrevOutIdx = 1,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "aff1644ed1675e54d5d0491c24259ebbc97d2791a2f6c1de18140679a9beb46e",
                                     _txInPrevOutIdx = 0,
                                     _txInScript =
                                       rawScriptFromString
                                         "47304402206f81af43689d9af71e8fa2674d215611f6e4d14569f7a17be6c01988b6f7608202202928cc0bd062d590afae9470dbcaf8ef8506ba62af6ba51c4825dafb631c8e05014104823e757b4ef0be00c0f58f33f54b91a69bbd0b90ddc55d1649b93f07676324abe598c84edbb616134feaa65c21e6e18b5004f99b1126c42276dabed0cbf778c6",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 4586000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a914e010cb78d29e1238d28c78dcfa9e16d1f795188188ac"},
                             TxOutput{_txOutValue = 102000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9145811cfd9bc24826c2ed456e14fbd79525387bc9d88ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "48f1ac1936de25270d1e3e35e2edf849509fe973c52c88447a92d479eac6979c"},
                       rawScriptFromString
                         "4930460221008d94fd7e87729e9bd61b08c7dd529850f798fbfa7db620a1ad6031e61b0c849c022100cce603da7ea742109834a38df1202afb5cbe8a9802c8a550e077874b1ba092420141046bb99bb9e71205eb3e993db2274069eba9bac5627e546685aeb01a5acdefc3d704ebafa1a7bff71c8c2b480cc77ee29410d830e3a199546ef7be0a8861b49cf4"),
                    _txInSeqNo = 4294967295}],
         _txOutputs =
           [TxOutput{_txOutValue = 223000000,
                     _txOutScript =
                       rawScriptFromString
                         "76a91407a2bc41a410629bf3055f6c73b0fc32aad822ac88ac"}],
         _txLockTime = LockImmed,
         _txHash =
           hash256FromTextBE
             "bdc8af818b11a0ca0b2f3a61b1de5cc2ce7b7067b112ccb46857f713f16bbae3"}


    tx_0b773101 = 
      Tx{_txVersion = 1,
         _txInputs =
           [TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "67197f1178c44b3fa7ef6610b70836a8b04af0bea1d427db509f5f7068e3539e",
                    _txInPrevOutIdx = 1,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "a7632a1d2316c08b0fcea0e6344c4f83366b96a7e6507f064bb36576ce0d1ad9",
                                     _txInPrevOutIdx = 0,
                                     _txInScript =
                                       rawScriptFromString
                                         "48304502203ab38d58c3bdb563aae2eb473f3a8f27a8867ef1f3274bccb91402fcec3a189e0221008fcc6232a3be6e6d318f8e5a4b03a020277c4cab5440afd7948fe9d21462ed950141048e8292b55f1f6bac5ac9bf1f3248778d13f654c0853030c372eeb2285bfb6cfebe1c9a4a0452d5e0659fab0ba2ea29c4c3fe262d8279973f8c8252fdb494ef7e",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 2064550000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9142315edf4844163b4deefd2131ac953a6ada81ce488ac"},
                             TxOutput{_txOutValue = 102000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9145811cfd9bc24826c2ed456e14fbd79525387bc9d88ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "67197f1178c44b3fa7ef6610b70836a8b04af0bea1d427db509f5f7068e3539e"},
                       rawScriptFromString
                         "49304502205d1d6cd5142d653191f35f79f8189003e872d9e0a989485dab9aebc226fe65a4022100aadaba6e50d418d7e13f1b40fabf56796b037ee14c9a15b58c9dc1055e09bc1c000141046bb99bb9e71205eb3e993db2274069eba9bac5627e546685aeb01a5acdefc3d704ebafa1a7bff71c8c2b480cc77ee29410d830e3a199546ef7be0a8861b49cf4"),
                    _txInSeqNo = 4294967295}],
         _txOutputs =
           [TxOutput{_txOutValue = 102000000,
                     _txOutScript =
                       rawScriptFromString
                         "76a91407a2bc41a410629bf3055f6c73b0fc32aad822ac88ac"}],
         _txLockTime = LockImmed,
         _txHash =
           hash256FromTextBE
             "389300df02f893990af44186a61da75ccf4ac2dcf93128ff413cb95c0131770b"}


    tx_ceafe873 = 
      Tx{_txVersion = 1,
         _txInputs =
           [TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "bed6395f6418a1583bd68f48aa0593dae6b47a765098dde25f4e955d6488b6fd",
                    _txInPrevOutIdx = 1,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "e6d403d46f7305df423192978763b62457d7ff8aebbe22b133b5bb8b52f4cb00",
                                     _txInPrevOutIdx = 0,
                                     _txInScript =
                                       rawScriptFromString
                                         "47304402200ad59ee68ab7cccee7ea26815ab296364ae13fc9f38e7a6e94399a22c1e65b8702203dde6ca242748c8d51d4b30aa1940b6b70d449ea80cde7c6f2a5aff636ed9a3c0141045cb179a60883b27adf93c4509213ac267c1bcf4ffbe39856ff70f2d6be61c548e59c4ae5fb6c49c7c7191eb95c1dbebf8af4712448d5a1f069cf83ae5de5ac58",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 4744900000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9144c6fa7fbdcb3959010e6191e66904e39f815b0a988ac"},
                             TxOutput{_txOutValue = 104000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9145811cfd9bc24826c2ed456e14fbd79525387bc9d88ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "bed6395f6418a1583bd68f48aa0593dae6b47a765098dde25f4e955d6488b6fd"},
                       rawScriptFromString
                         "493046022100e66b38c011255bb133aada82914e682dca5ee169c215e31ae3603b5b54b862f8022100aff39a14e80be20dbc98ae3528a5e42f4a898f089102c7817ddc33602f2797650141046bb99bb9e71205eb3e993db2274069eba9bac5627e546685aeb01a5acdefc3d704ebafa1a7bff71c8c2b480cc77ee29410d830e3a199546ef7be0a8861b49cf4"),
                    _txInSeqNo = 4294967295},
            TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "d1e55fdf8b71ad4aecc5cc12840257d28e3b40c1ff9c6b2a7e2976deeb6611d2",
                    _txInPrevOutIdx = 1,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "5a65a7beb2c478d048c3eb18444ff9204f5f77a8fb3f5dbfb3f4e49b57f16b96",
                                     _txInPrevOutIdx = 1,
                                     _txInScript =
                                       rawScriptFromString
                                         "493046022100a6ac7e801d280eaf16339a078c33c7f712a0394e7ccecee306265c3bf60acc0c022100e221472d77ac1775268e4884e72eb5e5cc5b9c2b0951f1adc95ee9f92ef71063014104a4ca069784ba620949dd951a73334902532b44ff39114a4bf167f2a35ff41ad4550835caaabe1eabd2db51e13bfd46cee79bb663a0d4dd23cad01f25f2613615",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 4795950000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a914ba1ca31dbab0e3870f4a263d2a4f0907f7b7bd2b88ac"},
                             TxOutput{_txOutValue = 104000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9145811cfd9bc24826c2ed456e14fbd79525387bc9d88ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "d1e55fdf8b71ad4aecc5cc12840257d28e3b40c1ff9c6b2a7e2976deeb6611d2"},
                       rawScriptFromString
                         "493046022100ff645e00619e1916d8cc7be33913108a0656194c45bd63cea132f12aa715edaa022100c75f64d96be527c3eddc7acac988cf3b1796c8c92e1918e71384dfb5c31ea0380141046bb99bb9e71205eb3e993db2274069eba9bac5627e546685aeb01a5acdefc3d704ebafa1a7bff71c8c2b480cc77ee29410d830e3a199546ef7be0a8861b49cf4"),
                    _txInSeqNo = 4294967295},
            TxInput{_txInPrevOutHash =
                      hash256FromTextBE
                        "ea8dad09e629bab7f70959938603b387607b3dd0429a0354a06ba26db4662d5e",
                    _txInPrevOutIdx = 1,
                    _txInScript =
                      (Tx{_txVersion = 1,
                          _txInputs =
                            [TxInput{_txInPrevOutHash =
                                       hash256FromTextBE
                                         "58ea922bda7baeb2eaf1e87770e02a553e95291f9d48aeea7a2394ed6cf15352",
                                     _txInPrevOutIdx = 0,
                                     _txInScript =
                                       rawScriptFromString
                                         "493046022100db7ac6a669cc3b7d23233b75dab4bbf9d5c19492f3cdd04d97edc0f0b71af85a022100a978c9edd7c2ba361dba5bccbda67bd169bb415ee9dcd13a2f0bc997477132fb0141042420ec3a32c7270c1ebee82876fdaf5995a6fa27b08f040e0274e79a8b7f7c4169141ef10aa79d14412cd0e3accef36064ca44809da324bb32a6115101b193d9",
                                     _txInSeqNo = 4294967295}],
                          _txOutputs =
                            [TxOutput{_txOutValue = 158850000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9143a0ff9a89e041243e08c922ec33fbe5b9f27394b88ac"},
                             TxOutput{_txOutValue = 101000000,
                                      _txOutScript =
                                        rawScriptFromString
                                          "76a9145811cfd9bc24826c2ed456e14fbd79525387bc9d88ac"}],
                          _txLockTime = LockImmed,
                          _txHash =
                            hash256FromTextBE
                              "ea8dad09e629bab7f70959938603b387607b3dd0429a0354a06ba26db4662d5e"},
                       rawScriptFromString
                         "493045022025191f4eb012217583450e687dac1aded6a5b8c6fe9d9893d854eb2359f9debe022100fbd1fa96c1cb8a7fbe186fe52d080d8709bd2bc44143d1d02a5c4a14f1a098dd000141046bb99bb9e71205eb3e993db2274069eba9bac5627e546685aeb01a5acdefc3d704ebafa1a7bff71c8c2b480cc77ee29410d830e3a199546ef7be0a8861b49cf4"),
                    _txInSeqNo = 4294967295}],
         _txOutputs =
           [TxOutput{_txOutValue = 309000000,
                     _txOutScript =
                       rawScriptFromString
                         "76a91407a2bc41a410629bf3055f6c73b0fc32aad822ac88ac"}],
         _txLockTime = LockImmed,
         _txHash =
           hash256FromTextBE
             "0f01472e25c7e6918f9ae36c6c0920e27d9cad1cab095ed7421f029773e8afce"}



--------------------------------------------------------------------------------

