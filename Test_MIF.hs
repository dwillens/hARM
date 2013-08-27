module Main where
  import MIF

  import Data.Word
  import Test.HUnit

  mif = "DEPTH = 20;\n\
        \WIDTH = 32;\n\
        \ADDRESS_RADIX = HEX;\n\
        \DATA_RADIX = HEX;\n\
        \CONTENT\n\ 
        \BEGIN\n\
        \00 : e3a00041;\n\
        \04 : eb000004;\n\
        \08 : eafffff4;\n\
        \0c : e3a01cff;\n\
        \10 : e3a0200a;\n\
        \14 : e312000a;\n\
        \18 : 01a0f00e;\n\
        \1c : e5d13000;\n\
        \20 : e0033002;\n\
        \24 : e3130002;\n\
        \28 : 15c10002;\n\
        \2c : 13a00002;\n\
        \30 : 15c10000;\n\
        \34 : 13c22002;\n\
        \38 : e3130008;\n\
        \3c : 15c10004;\n\
        \40 : 13a00008;\n\
        \44 : 15c10000;\n\
        \48 : 13c22008;\n\
        \4c : eaffffc4;\n\
        \END;\n"

  ws :: [Word32]
  ws = [0xE3A00041
       ,0xEB000004
       ,0xEAFFFFF4
       ,0xE3A01CFF
       ,0xE3A0200A
       ,0xE312000A
       ,0x01A0F00E
       ,0xE5D13000
       ,0xE0033002
       ,0xE3130002
       ,0x15C10002
       ,0x13A00002
       ,0x15C10000
       ,0x13C22002
       ,0xE3130008
       ,0x15C10004
       ,0x13A00008
       ,0x15C10000
       ,0x13C22008
       ,0xEAFFFFC4
       ]

  main = runTestTT $ TestList $ map TestCase
    [assertEqual "to MIF" mif $ showMIF ws
    ,assertEqual "from MIF" ws $ readMIF mif
    ]
