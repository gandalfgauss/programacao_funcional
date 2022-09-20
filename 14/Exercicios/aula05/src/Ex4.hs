module Ex4 where

    type Name = String
    type Surname = String
    type SendOffer =  Bool

    data Client =
        Customer {
            name :: Name
            , surname :: Surname
            , offer :: SendOffer
        }
        deriving Show

    ex4 ::Client
    ex4 = Customer "Jose" "Silva" False

    func :: [Client] -> [Client]
    func lista = [cliente | cliente <- lista, offer cliente]