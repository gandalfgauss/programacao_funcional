module Ex2 where
    andList :: [Bool] -> Bool
    andList [x] = x
    andList (x:xs) = x && andList xs

    orList :: [Bool] -> Bool
    orList [x] = x
    orList (x:xs) = x || orList xs