module Ex3 where
    indexOf :: Int -> [Int] -> Int
    indexOf x y = indexOf2 x y 0
        where
            indexOf2 x [] _ = -1
            indexOf2 x (y:ys) i 
                | x == y = i
                | otherwise = indexOf2 x ys (i+1)