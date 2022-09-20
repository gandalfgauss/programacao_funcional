module Ex2 where
    data Tree a = Leaf | Node a (Tree a) (Tree a)
        deriving (Eq, Ord, Show)
    
    foldTree :: (a -> b -> b -> b) -> b -> Tree a -> b
    foldTree _ v Leaf = v
    foldTree f v (Node x l r) = f x (foldTree f v l) (foldTree f v r)
    
    numElements ::  Tree a -> Int
    numElements = foldTree (\ v acl acr -> 1+ acl+ acr) 0