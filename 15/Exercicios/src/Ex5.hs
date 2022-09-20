module Ex5 where
    takeWhile2 :: (a -> Bool) -> [a] -> [a]
    takeWhile2 _ [] = []
    takeWhile2 p (x : xs)
        | p x = x : takeWhile2 p xs
        | otherwise = []

    takeWhile3 :: (a -> Bool) -> [a] -> [a]
    takeWhile3 f = foldr step []
        where
            step x1 ac
                | f x1 = x1 :ac
                | otherwise = ac