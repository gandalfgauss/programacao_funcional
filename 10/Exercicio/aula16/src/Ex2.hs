module Ex2 where
    data Toy a b = Output a b | Bell b | Done

    {-
    :k Toy =
        Toy :: * -> * -> *
    -}

    instance Functor Toy where
        fmap _ Done = Done
        fmap f (Bell b) = Bell (f b)
        fmap f (Output a b) = (Output (f a) (f b))

    {-
        Em teoria a instancia de Functor para Toy
        estaria representada acima
        Mas a instacia de Functor requer um kind
        de * -> * e o construtor toy tem kind de
        * -> * -> * 
    -}