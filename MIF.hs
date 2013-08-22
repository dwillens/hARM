module MIF (MIF(..)) where
   import Data.Bits
   import Text.Printf
   import Data.Word
   
   newtype MIF a = MIF { getBuffer :: [a] }

   instance (PrintfArg a, Num a, Bits a) => Show (MIF a) where
      show (MIF is) =
         concat [mifHeader
                ,unlines $ zipWith (printf mifFormat) addresses is
                ,mifFooter
                ]
         where width :: Int
               width = bitSize $ head is

               depth :: Int
               depth = length is

               mifHeader :: String
               mifHeader = unlines ["DEPTH = " ++ show depth  ++ ";"
                                   ,"WIDTH = " ++ show width ++ ";"
                                   ,"ADDRESS_RADIX = HEX;"
                                   ,"DATA_RADIX = HEX;"
                                   ,"CONTENT"
                                   ,"BEGIN"
                                   ]

               mifFooter :: String
               mifFooter = "END;\n"

               addresses :: [Integer]
               addresses = [0,(fromIntegral width) `div` 8..]

               mifFormat :: String
               mifFormat = "%0" ++ show (intLog 16 lastAddress) ++ "x : " ++
                           "%0" ++ show (width `div` 4) ++ "x;"
                  where lastAddress = head $ drop (depth - 1) addresses


               intLog :: (Integral a) => a -> a -> a
               intLog b n 
                  | n < b = 1
                  | otherwise = 1 + intLog b (n `div` b)