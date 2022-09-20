module Ex1 where

    contaLinhas :: String -> Int
    contaLinhas [] = 1
    contaLinhas (letra :dados) 
        | letra == '\n' = 1 + contaLinhas dados
        | otherwise = contaLinhas dados

    contaPalavras :: String -> Int
    contaPalavras dado = contaPalavras2 dado False
    
    contaPalavras2 :: String -> Bool-> Int
    contaPalavras2 [] _ = 0
    contaPalavras2 (d1:ds) letra
        | d1 /= ' ' && not letra = 1 + contaPalavras2 ds True
        | (d1 == ' ' || d1 == '\n') && letra = contaPalavras2 ds False
        | otherwise = contaPalavras2 ds letra 


    lerArquivo :: IO ()
    lerArquivo = 
        do
            nomeDoArquivo <- getLine
            dados <- readFile nomeDoArquivo
            putStrLn ("Quantidade de Palavras: " ++ show (contaPalavras dados))
            putStrLn ("Quantidade de Linhas: " ++ show (contaLinhas dados))
            
            







