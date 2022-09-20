module Ex2 where
    filter2 :: (a->Bool) -> [a]->[a]
    filter2 f xs = [y | y <- xs, f y]

    estahNoIntervalo :: Int -> (Int, Int) ->Bool
    estahNoIntervalo x (a, b) 
        | x >= a && x <=b = True
        | otherwise = False

    gerarLista :: [Int] -> (Int, Int) ->[Int]
    gerarLista xs (a, b) = filter estahNoIntervalo2 xs 
        where
            estahNoIntervalo2 z = estahNoIntervalo z (a,b)
    

    gerarListaRecursivo :: [Int] -> (Int, Int) ->[Int]
    gerarListaRecursivo [] _ = []
    gerarListaRecursivo (x:xs) (a, b) 
        |estahNoIntervalo x (a,b) = x: gerarListaRecursivo xs (a,b)
        |otherwise  = gerarListaRecursivo xs (a,b)