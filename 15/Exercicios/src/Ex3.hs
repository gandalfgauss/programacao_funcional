module Ex3 where


    func :: [Int] -> Int
    func (x:xs) = foldr (+) 0 (map (\y -> y^2) (filter (not . even) (x:xs)))
