module Q2 where
    data Question = Question [Subject] Statement [Choice] Value
        deriving Show


    type Subject = String
    type Statement = String

    data Choice = Choice String Bool
        deriving Show

    type Value = Int
    
    data Questionary = Questionary String Value [Question]
        deriving Show

    data AnswerSet = AnswerSet Student [Answer]
        deriving Show

    type Student = String
    
    data Answer = TheAnswer Int | Blank
        deriving Show