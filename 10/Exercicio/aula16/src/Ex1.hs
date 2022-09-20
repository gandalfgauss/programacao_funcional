module Ex1 where
    data Tree a = Leaf | Node a (Tree a) (Tree a)
        deriving (Eq, Ord, Show)

    data Lista a = Inil | Icons a (Lista a)
        deriving (Eq, Ord, Show)

    instance Functor Tree where
        fmap _ Leaf = Leaf
        fmap f (Node x l r)
            = Node (f x) (f <$> l) (f <$> r)

    instance Functor Lista where
        fmap _ Inil = Inil
        fmap f (Icons x xs)
            = Icons (f x) (f <$> xs)

    instance Applicative Tree where
        pure x = Node x (pure x) (pure x) 
        Leaf <*> _ = Leaf
        _ <*> Leaf= Leaf
        (Node f e d) <*> (Node x e' d') = Node (f x) (e <*> e') (d <*> d')

    instance Applicative Lista where
        pure x = Icons x (pure x)
        Inil <*> _ = Inil
        _ <*> Inil= Inil
        (Icons f e) <*> (Icons x e') = Icons (f x) (e <*> e')
    
