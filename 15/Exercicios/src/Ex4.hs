module Ex4 where
    
    concatena :: [a] -> [a] ->[a]
    concatena (x:xs) (y:ys) = (x:xs) ++  concatena2 (y:ys)
       
    concatena2 :: [a] -> [a]
    concatena2 = foldr (:) []