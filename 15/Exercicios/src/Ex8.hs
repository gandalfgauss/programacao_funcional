module Ex8 where
    concatMap2 :: (a -> [b]) -> [a] -> [b]
    concatMap2 f = foldr step []
        where 
            step x xs = f x ++ xs