module Ex1 where
    minList :: [Int] -> Int
    minList [x] = x
    minList (y:x:xs) = minList2 y (x:xs)
        where
            minList2 y [] = y
            minList2 y (x:xs) = minList2 (min y x) xs

    