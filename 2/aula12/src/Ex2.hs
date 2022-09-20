module Ex2 where
    data Person = Person { name::String
                            , age ::Int
                        }
    instance Eq Person where
        (Person nome _) == (Person nome' _) = 
            nome == nome'

        x /= y = not (x == y)