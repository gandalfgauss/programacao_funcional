module Ex6 where
    removeAll1 :: Int -> [Int] -> [Int]
    removeAll1 _ [] = []
    removeAll1 x (y:ys)
        | x /= y = y:removeAll1 x ys
        | otherwise = removeAll1 x ys

    removeAll2 :: Int -> [Int] -> [Int]
    removeAll2 x y = [a| a <- y, a /= x]
    