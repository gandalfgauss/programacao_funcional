module Ex1 where

    bools :: [Bool]
    bools = [True, False, True]

    nums :: [[Int]]
    nums = [[1,2,3], [4,5,6]]

    add :: Int -> Int -> Int -> Int
    add a b c = a+b+c

    copy :: a -> (a,a)
    copy t = (t,t)

    apply :: (a-> b) -> a -> b
    apply copy a = (copy a)

    swap :: (a,b) -> (b,a)
    swap (a,b) = (b,a)
