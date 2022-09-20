module Ex5 where
    dropList :: Int -> [a] ->[a]
    dropList 0 xs = xs
    dropList _ [] = []
    dropList n (x:xs) = dropList (n-1) xs