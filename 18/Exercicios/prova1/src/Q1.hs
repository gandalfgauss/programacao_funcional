module Q1 where
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

    type QuestionDB = [Question]

    data Class = Class Questionary [AnswerSet]
        deriving Show

    type Report = [(Student, Int)]

    type ColumnSize = (Int, Int)
    {-----------------testes-----------------------------------------}
    question1 :: Question
    question1 = Question ["História"] stmt choices 1
        where
            stmt = "Qual a cor do cavalo branco de Napoleão?"
            choices = [ch1, ch2, ch3]
            ch1 = Choice "Azul" False
            ch2 = Choice "Preto" False
            ch3 = Choice "Branco" True
    
    
    questionf1 :: Question
    questionf1 = Question ["História"] stmt choices 1
        where
            stmt = "Qual a cor do cavalo branco de Napoleão?"
            choices = [ch1, ch2, ch3]
            ch1 = Choice "Azul" False
            ch2 = Choice "Preto" True
            ch3 = Choice "Branco" True
        
    questionf2 :: Question
    questionf2 = Question ["História"] stmt choices 0
        where
            stmt = "Qual a cor do cavalo branco de Napoleão?"

            choices = [ch1, ch2, ch3]
            ch1 = Choice "Azul" False
            ch2 = Choice "Preto" False
            ch3 = Choice "Branco" True
    
    questionf3 :: Question
    questionf3 = Question ["História"] stmt choices (-2)
        where
        stmt = "Qual a cor do cavalo branco de Napoleão?"
        choices = [ch1, ch2, ch3]
        ch1 = Choice "Azul" False
        ch2 = Choice "Preto" False
        ch3 = Choice "Branco" True
    {-----------------testes-----------------------------------------}

    {-
    Questao 1
        A ideia eh chamar a funcao wfQuestion2 passando somente a lista de Choice e um contador
        Essa funcao Vai retornar quantas alternativas verdadeiras a questao possui 
        Entao serah comparado se a quantidade de alternativas verdadadeira eh 1 
        e o valor da questao nao eh nuloou negativo


        A funcao wfQuestion2 faz o que foi supracitado e usa wfQuestion3 para conferir
        se um alternativa eh verdadeira, e retorna 1 se True e 0 se False
        a funcao wfQuestion 2 eh chamada recursivamente para cada elemento da lista
    -}

    wfQuestion :: Question -> Bool
    wfQuestion (Question listaSub enunciado choices valor)
        | (wfQuestion2 choices 0) == 1 && valor > 0 = True
        | otherwise = False
    


    wfQuestion2 :: [Choice] -> Int ->Int
    wfQuestion2 [] contador = contador
    wfQuestion2 (x:xs) contador = (wfQuestion2 xs (contador+(wfQuestion3 x))) 
        where
            wfQuestion3  (Choice nome certoErrado) 
                | certoErrado = 1
                | otherwise = 0

    
    {-----------------testes-----------------------------------------}
    question11 :: Question
    question11 = Question ["História"] stmt choices 5
        where
            stmt = "Qual a cor do cavalo branco de Napoleão?"
            choices = [ch1, ch2, ch3]
            ch1 = Choice "Azul" False
            ch2 = Choice "Preto" False
            ch3 = Choice "Branco" True

    question2 :: Questionary
    question2 = Questionary "Teste de história" 6 [question11, question1]

    question2a :: Questionary
    question2a = Questionary "Teste de história"6 [question11, questionf1]

    question2b :: Questionary
    question2b = Questionary "Teste de história" 6 [question11]
    {-----------------testes-----------------------------------------}


    {-
        Questao 2
        A ideia de wfQuestionary eh utilizar uma funcao para conferir se as questoes sao bem formadas(wfQuestionary2)
        e uma outra funcao para conferir se o total de pontos do questionario eh igual
        a soma dos pontos das questoes

        wfQuestionary2 recebe uma lista de questoes e aplica a funcao wfQuestion ja implementada
        para conferir se um questao eh bem forma e applica o 'e' logico sobre todos os resutados
        se uma questao nao for bem formada a resposta eh False
    -}

    wfQuestionary :: Questionary -> Bool
    wfQuestionary (Questionary nome total questoes) 
        | (wfQuestionary2 questoes) && (totalQuestionario questoes == total) = True
        | otherwise = False


    wfQuestionary2 :: [Question] ->Bool
    wfQuestionary2 [] = True
    wfQuestionary2 (x : xs) = (wfQuestion x) && wfQuestionary2 xs

    {-
    A funcao totalQuestionario vai simplesmente percorrer a lista de questoes e retorna a soma dos valores das mesmas
    -}
    totalQuestionario :: [Question] -> Int
    totalQuestionario [] = 0
    totalQuestionario (x:xs) = totalQuestionario' x +totalQuestionario xs
        where 
            totalQuestionario' (Question sub enunciado choices valor) = valor

    {-----------------testes-----------------------------------------}
    answer11 :: AnswerSet
    answer11 = AnswerSet "João da Silva" [TheAnswer 2, TheAnswer 2]

    answer12 :: AnswerSet
    answer12 = AnswerSet "João da Silva" [TheAnswer 2]
    {-----------------testes-----------------------------------------}

    
    {-
        Questao 3

        Se a lista de questoes do questionario for menor igual a lista de respostas Answer
        do aluno então a solução do aluno possui um valor do tipo Answer para cada valor do tipo Question
    -}

    wfAnswerSet :: Questionary -> AnswerSet -> Bool
    wfAnswerSet (Questionary nome valor questoes) (AnswerSet nomeEstudante respostas) 
        | length questoes <= length respostas = True
        | otherwise = False


    {-----------------testes-----------------------------------------}
    answer13 :: AnswerSet
    answer13 = AnswerSet "João da Silva" [TheAnswer 2, Blank]
    {-----------------testes-----------------------------------------}

    {-
        Questao 4
        A ideia da funcao countBlanks eh simplesmente somar 1 quando o elemento 
        da cbeca da lista eh Blank e somar 0 caso contrario, isso acontece
        ateh chegar na lista vazia onde eh retornado 0
    -}

    countBlanks :: AnswerSet -> Int
    countBlanks (AnswerSet nome []) = 0
    countBlanks (AnswerSet nome (Blank : xs)) = 1 + countBlanks (AnswerSet nome xs)
    countBlanks (AnswerSet nome (x:xs)) = 0 + countBlanks (AnswerSet nome xs)

    {-----------------testes-----------------------------------------}
    questionDB :: QuestionDB
    questionDB = [ question1, question5]
    question5 :: Question
    question5 = Question ["Math"] stmt choices 5
        where
        stmt = "Quanto é 2 + 2?"
        choices = [ch1, ch2, ch3]
        ch1 = Choice "0" False
        ch2 = Choice "22" False
        ch3 = Choice "4" True
    {-----------------testes-----------------------------------------}
        
    {-
        Questao 5
        
        A ideia eh retorna no 'then' a questao e a juntar na lista se a condicao do if eh verdadeira, 
            e nao juntar na lista de questoes se a condicao do if eh falsa
            A condicao do if visa por meio do 'filter' percorrer a lista de topicos de uma questao
            e se o topico procurado for igual ao topico analisado o topico eh retornado
            ou seja, o 'filter' vai retornar uma lista de topicos que eh igual ao procurado,
             em seguida. o tamanho da lista eh calculado para conferir se tem algum topico igual
             ao procurado
    -}

    selectBySubject :: Subject -> QuestionDB -> [Question]
    selectBySubject topico = foldr step base
        where
            step = ((\(Question (x:xs) enunciado choices valor) -> if (length (filter (\z -> z == topico) (x:xs))) > 0 then ((Question (x:xs) enunciado choices valor):) else id))
            base = []

    
    {-
        Questao 6

        A funcao grade recebe um questionario e uma resposta do aluno
        entao chama a funcao grade2 e passa como parametro as questoes respondidas
        e as respostas.

        A funcao grade realiza a soma da pontuação nas questões
        através da funcao verificarCorreto
        percorrendo as listas de questoes e respostas

        A funcao verificar correto recebe como parametro uma questao e uma resposta
        se a questao estiver em branco retorna nota 0
        caso contrario chama a funcao verificarCorreto2 
        que por sua vez confere confere se foi marcado a resposta corretamente
        e retorna bool se o estudante acertou a questao e falso caso contrario
        Se o estudante acertou a questao eh retornado o valor da questao
        caso contrario eh retornado nota 0

    -}

    grade :: Questionary -> AnswerSet -> Int
    grade (Questionary nome valor questoes) (AnswerSet nomeEstudante respostas) = grade2 questoes respostas

    grade2 :: [Question] -> [Answer] -> Int
    grade2 [] _ = 0
    grade2 _ [] = 0
    grade2 (q:qs) (r:rs) = (verificarCorreto q r) + grade2 qs rs

    
    verificarCorreto :: Question -> Answer -> Int
    verificarCorreto questao Blank  = 0
    verificarCorreto (Question nome topicos choices valor) (TheAnswer resposta) 
        | verificarCorreto2 choices resposta 0 =  valor
        | otherwise = 0

    verificarCorreto2 :: [Choice] -> Int-> Int ->Bool
    verificarCorreto2 [] _ _ = False
    verificarCorreto2 ((Choice nome boleano):cs) resposta cont
        | cont < resposta = verificarCorreto2 cs resposta (cont+1)
        | cont == resposta && boleano = True
        | otherwise = False

{-----------------testes-----------------------------------------}

    exampleClass :: Class
    exampleClass = Class questionary answers

    questionary :: Questionary
    questionary = Questionary "Math" 10 [questiona, questionb]

    questiona :: Question
    questiona = Question ["Math"] stmt choices 5
        where
            stmt = "Quanto é 2 + 2?"
            choices = [ch1, ch2, ch3]
            ch1 = Choice "0" False
            ch2 = Choice "22" False
            ch3 = Choice "4" True
    
    questionb :: Question
    questionb = Question ["Math"] stmt choices 5
        where
            stmt = "Quanto é 1 + 1?"
            choices = [ch1, ch2, ch3]
            ch1 = Choice "0" False
            ch2 = Choice "11" False
            ch3 = Choice "2" True
    answerj :: AnswerSet
    answerj = AnswerSet "João da Silva" [TheAnswer 1, Blank]

    answerm :: AnswerSet
    answerm = AnswerSet "Ana Maria" [TheAnswer 2, TheAnswer 2]
   
    answers :: [AnswerSet]
    answers = [answerj, answerm]


{-----------------testes-----------------------------------------}

{-
    Questao 7

    A funcao create reporte chama a funcao createReport2 passando o questionario
    e uma lista de resposta de alunos como parametro

    E a funcao creatReport2 cria uma tupla com o nome de um estudante 
    e a nota que tirou na prova usando a funcao grade implementada anteriormente
-}

    createReport :: Class -> Report
    createReport (Class questionario respostas) = createReport2 questionario respostas

    createReport2 :: Questionary -> [AnswerSet] -> Report
    createReport2 _ [] =[]
    createReport2 questionario ((AnswerSet nomeEstudante respostasEstudante):xs) = 
        (nomeEstudante, grade questionario (AnswerSet nomeEstudante respostasEstudante)): (createReport2 questionario xs)

    

    {-
    Questao 8
        A funcao columnsSize chama a funcao comlumnsSize2 para calcular o tamanho maximo da coluna de nomes
        e a funcao comlumnsSize3 para calcular o tamanho maximo da nota como caractere

        a funcao columsSize para cada par (estudante e nota) pega o estudante e conta a quantidade
        de caracteres que possu no nome e armazena numa lista, isso acontece para cada estudante, 
        então eh calculado o maximo, ou seja, o tamanho do maior nome da lista de estudantes, 
        se o tamanho for menor que 4 retorna 4 caso contrario retorna o tamanho
        pois a tabela tem que caber o cabecalho

        Para calcular o tamanho da nota o procedimento eh parecido so muda em como a contagem 
        de caracteres eh feita, ela feita diividindo por 10 ate que se encontre 0, e em cada divisao
        eh somado em um, por fim o tamanho do numero eh retornado

    -}

    columnsSize :: Report -> ColumnSize
    columnsSize x = (columnsSize2 x, columnsSize3 x)

    columnsSize2 :: Report -> Int
    columnsSize2 x = aux (maximum [contarCaractere estudante | (estudante, _) <- x])
        where
            aux y = if y > 4 then y else 4

    contarCaractere :: [Char] -> Int
    contarCaractere [] = 0
    contarCaractere (z:zs) = 1 + contarCaractere zs
    
    columnsSize3 :: Report -> Int
    columnsSize3 x = aux (maximum [contarNota nota | (_, nota) <- x])
        where
            aux w = if w > 4 then w else 4

    contarNota :: Int -> Int
    contarNota 0 = 0
    contarNota z = 1 + contarNota (div z 10)
    
    {-
    Questao 9
        A funcao line reporta monta a tabela e utiliza as funcoes implementadas
        anteriormente para contar a quantidade de caracteres que os dados
        (tanto nota como nome tem) ppara saber se eles sao os maiores elementos da tabela
        e chama a funcao adicionar espacos que vai adicionar os espaco necessarios caso 
        os dados passados nao sejam os maiores dados da tabela, isso eh feito para
        nao desconfigurar a tabela


    -}

    lineReport :: (Student, Int) -> ColumnSize -> String
    lineReport (nomeEstudante, nota) (nomeColuna, notaColuna) = "| " ++ nomeEstudante 
        ++ (adicionaEspacos(contarCaractere nomeEstudante) nomeColuna) ++ " | " ++ (show nota)  ++ (adicionaEspacos(contarNota nota) notaColuna) ++ " |"
    
    adicionaEspacos :: Int-> Int -> String
    adicionaEspacos caracteresEstudante disp
        | caracteresEstudante >= disp = ""
        | otherwise = adicionaEspacos2 (disp-caracteresEstudante)
        where
            adicionaEspacos2 0 = ""
            adicionaEspacos2 cont = " " ++ adicionaEspacos2 (cont-1)

    {-
        Questao 10
        A funcao printReport recebe uma classe e chama a funcao criaReporte que por sua vez 
        chama uma funcao jah implementada("createReport") e chama a funcao columnsSize no reporte
        criado para calcula as dimensoes da tabela, em seguida esssas informacoes sao passadas como
        parametro da funcao imprime que atraves das funcoes auxiliares imprime2 e linereport, essa ultima ja implementada,
        imprime a tabela de maneira adequada
    -}

    printReport :: Class -> String
    printReport classe = imprime criarReporte (columnsSize criarReporte)
        where
            criarReporte = createReport classe
        
    
    imprime :: Report -> ColumnSize -> String
    imprime (x :xs) (nomeColuna, notaColuna) = "+" ++ (concat  (replicate (nomeColuna+2) "-")) ++ "+"
        ++ (concat  (replicate (notaColuna+2) "-") ) ++ "+\n" ++ "| " ++ "Nome" ++ (adicionaEspacos (contarCaractere "Nome") nomeColuna) 
        ++ " | " ++ "Nota" ++ (adicionaEspacos (contarCaractere "Nome") notaColuna) ++ " |\n" ++ "+" ++ (concat  (replicate (nomeColuna+2) "-")) ++ "+"
        ++ (concat  (replicate (notaColuna+2) "-") ) ++ "+\n"
        ++ (imprime2 (x:xs) (nomeColuna, notaColuna))
    
    
    imprime2 :: Report-> ColumnSize -> String
    imprime2 [] _ = ""
    imprime2 (x:xs) (nomeColuna, notaColuna) =  (lineReport x  (nomeColuna, notaColuna)) ++ "\n" ++ "+" ++ (concat  (replicate (nomeColuna+2) "-")) ++ "+"
        ++ (concat  (replicate (notaColuna+2) "-")) ++ "+\n"++ imprime2 xs  (nomeColuna, notaColuna)

    
    