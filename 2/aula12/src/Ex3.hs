module Ex3 where
    data Person = Person { name::String
                            , age ::Int
                        }
    instance Show Person where
        show (Person nome _) = nome 