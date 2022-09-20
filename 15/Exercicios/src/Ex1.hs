module Ex1 where
    map2 :: (a->b) -> [a]->[b]
    map2 f xs = [f y | y <- xs]

    evenList :: [Int] -> [Bool]
    evenList xs = map2 even xs