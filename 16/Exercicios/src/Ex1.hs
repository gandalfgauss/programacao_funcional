module Ex1 where

    import Data.Char(chr, ord, isLower)

    data Bit = O | I
        deriving Show


    multiplicaBit :: Int -> Bit -> Int
    multiplicaBit x I = x
    multiplicaBit x O = 0

    make8 :: [Bit] -> [Bit]
    make8 bs = take 8 (bs ++ repeat O)

    chop8 :: [Bit] -> [[Bit]]
    chop8 [] = []
    chop8 bs = take 8 bs : chop8 (drop 8 bs)

    bin2Int :: [Bit] -> Int
    bin2Int bs = foldr (+) 0 [(multiplicaBit w b) | (w,b) <- zip weights bs]
        where
            weights = iterate (* 2) 1
    
    int2Bin :: Int -> [Bit]
    int2Bin 0 = []
    int2Bin n 
        | mod n 2 == 0 = O : int2Bin (div n 2)
        | otherwise = I : int2Bin (div n 2)

    encode :: String -> [Bit]
    encode = concat . map (make8 . int2Bin . ord)
    
    decode :: [Bit] -> String
    decode = map (chr . bin2Int) . chop8