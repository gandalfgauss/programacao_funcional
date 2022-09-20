module Ex3 where

    data IntTree = ILeaf | INode Int IntTree IntTree
        deriving Show

    data IntList = INil | ICons Int IntList
        deriving Show
        

    insereNaArvore :: Int -> IntTree -> IntTree
    insereNaArvore x ILeaf = (INode x ILeaf ILeaf)
    insereNaArvore x (INode y esq dir) 
        | x > y = (INode y esq (insereNaArvore x dir))
        | x < y = (INode y (insereNaArvore x esq) dir)
        | otherwise = (INode y esq dir)

    converte :: IntList -> IntTree
    converte lista = converte2 lista ILeaf
    
    converte2 :: IntList -> IntTree -> IntTree
    converte2 INil arvore = arvore
    converte2 (ICons x resto) arvore = converte2 resto (insereNaArvore x arvore)
    
