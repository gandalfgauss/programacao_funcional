module Ex7 where
    countPos :: [Int] -> Int
    countPos x = countPos2 x 0
        where
            countPos2 [] cont = cont   
            countPos2 (y:ys) cont 
                | y > 0 = countPos2 ys cont+1
                |otherwise = countPos2 ys cont