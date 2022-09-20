module Ex6 where
    allRecursivo :: (a -> Bool) -> [a] -> Bool
    allRecursivo _ [] = True
    allRecursivo f (x:xs) = (f x) && allRecursivo f xs

    allFold :: (a -> Bool) -> [a] -> Bool
    allFold f  = foldr step True
        where
            step x xs = f x && xs