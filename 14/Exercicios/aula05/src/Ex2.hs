module Ex2 where
    data IntTree = ILeaf | INode Int IntTree IntTree

    numeroDeElementos :: IntTree -> Int
    numeroDeElementos ILeaf = 0
    numeroDeElementos (INode _ esq dir)  = (numeroDeElementos esq) + 
                                            (numeroDeElementos dir) +1 
  

    numeroDeFolhas :: IntTree -> Int
    numeroDeFolhas (INode _ ILeaf ILeaf) = 1
    numeroDeFolhas (INode _ esq dir)  = (numeroDeFolhas esq) + 
                                            (numeroDeFolhas dir)