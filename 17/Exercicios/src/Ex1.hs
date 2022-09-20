module Ex1 where
    data Tree a = Leaf | Node a (Tree a) (Tree a)
        deriving (Eq, Ord, Show)
    
    foldTree :: (a -> b -> b -> b) -> b -> Tree a -> b
    foldTree _ v Leaf = v
    foldTree f v (Node x l r) = f x (foldTree f v l) (foldTree f v r)
    
    mapTree2 :: (a->b) -> Tree a -> Tree b
    mapTree2 f = foldTree (\ v acl acr -> Node (f v) acl acr) Leaf