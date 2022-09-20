Avaliação 2 de Programação Funcional
========================

ATENÇÃO
-------

* A interpretação dos enunciados faz parte
da avaliação.

* A avaliação deve ser resolvida INDIVIDUALMENTE.
Plágios não serão tolerados. TODAS as avaliações
em que algum tipo de plágio for detectado receberão
nota ZERO.

* Se você utilizar recursos disponíveis na internet
e que não fazem parte da bibliografia, você deverá
explicitamente citar a fonte apresentando o link
pertinente como um comentário em seu código.

* Todo código produzido por você deve ser acompanhado
por um texto explicando a estratégia usada para a
solução. Lembre-se: meramente parafrasear o código
não é considerado uma explicação!

* Não é permitido eliminar a diretiva de compilação -Wall
do cabeçalho desta avaliação.

* Caso julgue necessário, você poderá incluir bibliotecas
adicionais incluindo "imports" no cabeçalho deste
módulo.

* Seu código deve ser compilado sem erros e warnings
de compilação. A presença de erros acarretará em
uma penalidade de 20% para cada erro de compilação e
de 10% para cada warning. Esses valores serão descontados
sobre a nota final obtida pelo aluno.

* Todo o código a ser produzido por você está marcado
usando a função "undefined". Sua solução deverá
substituir a chamada a undefined por uma implementação
apropriada.

* Todas as questões desta avaliação possuem casos de
teste para ajudar no entendimento do resultado
esperado. Para execução dos casos de teste, basta
executar os seguintes comandos:

```
$> stack build
$> stack exec prova02-exe
```

* Sobre a entrega da solução:

1. A entrega da solução da avaliação deve ser feita
como um único arquivo .zip contendo todo o projeto
stack usado.

2. O arquivo .zip a ser entregue deve usar a seguinte
convenção de nome: MATRÍCULA.zip, em que matrícula é
a sua matrícula. Exemplo: Se sua matrícula for
20.1.2020 então o arquivo entregue deve ser
2012020.zip. A não observância ao critério de nome e
formato da solução receberá uma penalidade de 20%
sobre a nota obtida na avaliação.

3. O arquivo de solução deverá ser entregue usando a
atividade "Entrega da Avaliação 2" no Moodle dentro do
prazo estabelecido.

4. É de responsabilidade do aluno a entrega da solução
dentro deste prazo.

5. Sob NENHUMA hipótese serão aceitas soluções fora do
prazo ou entregues usando outra ferramenta que
não a plataforma Moodle.


Setup inicial
-------------

> {-# OPTIONS_GHC -Wall #-}

> module Main where

> import ParseLib
> import Test.Tasty
> import Test.Tasty.HUnit

> main :: IO ()
> main = defaultMain tests

> tests :: TestTree
> tests
>   = testGroup "Unit tests"
>         [
>            question01aTests
>         ,  question01bTests
>         ,  question02aTests
>         ,  question02bTests
>         ,  question03aTests
>         ,  question03bTests
>         ]

> parse :: Parser s a -> [s] -> Maybe a
> parse p s = if null res then Nothing
>             else Just $ fst (head res)
>      where
>        res = runParser p s

Turtle-oriented programming
===========================

Introdução
-----------

Nesta avaliação, você deverá implementar um conjunto de funções para realizar o parsing de
uma pequena linguagem de programação para movimentar uma tartaruga em um plano 2D.

Desenvolvimento do parser
-------------------------

Questão 1. Nessa questão desenvolveremos um conjunto de funções para
realizar o parsing de uma posição da tartaruga no plano. Posições são
representadas pelo seguinte tipo:

> data Position
>       = Position {
>            x :: Int
>         ,  y :: Int
>         } deriving (Eq, Ord, Show)

que representa as coordenadas da tartaruga no plano cartesiano.

a) Desenvolva o parser:

> parseMenos :: Parser Char [Char]
> parseMenos = f <$>  greedy (symbol '-')
>   where
>       f menos = menos




> parseNumber :: Parser Char Int
> parseNumber = f <$> whitespace <*> parseMenos <*> natural <*> whitespace
>   where
>       f _ menos numero _ = read (menos ++ show numero) :: Int

que processa números inteiros descartando espaços antes e depois da
string processada. Sua implementação deve satisfazer os seguintes
casos de teste.

-----------------------Comentario 1 a) ----------------------------------------------------------------------------------
A ideia eh extrair de uma string um numero inteiro, tanto positivo, quanto negativo. Ou seja, fazer um Parsing.
Entao foi implementada a funcao 'parseMenos' que eh responsavel por extrair os simbolos de '-' da string utilizando
o combinador greedy e o Parser symbol. Nessa funcao todos os simbolos de '-' sao extraidos da string na sequencia. 
Se nao tiver simbolo de '-' nao eh extraido nada da string, ou seja o numero eh positivo.

Em seguida a funcao parseNumber foi implementada. Essa funcao limpa os espaços em branco utilizando o Parser
whitespace e o restante a ser processado da string eh processado no Parser parseNumber, que foi supracitado,
na sequencia utilizando o Parser natural o numero eh extraido sem o sinal, e por ultimo os espaços em branco
ao final da string sao removidos. Feito isso a funcao 'f' que eh um fmap que pegarah
o resultado(string) do Parser menos e o resultado do Parser natural, transformando o inteiro do Parser natural em string,
utilizando show, concatenando o sinal com o numero, e entao a funcao read eh utilizada no intuito de converter a string
no numero correspondente. E eh isso que eh retornado no Parse parseNumber, um numero inteiro e
o restante nao processado da string.

Nesse site abaixo encontrei como utilizar read
https://programming-idioms.org/idiom/22/convert-string-to-integer/802/haskell

> question01aTests :: TestTree
> question01aTests
>       = testGroup "Question 01-a Tests"
>                    [
>                       testCase "Question 01-a success" $
>                           parse parseNumber "11" @?= Just 11
>                    ,  testCase "Question 01-a success spaces right" $
>                           parse parseNumber "11 " @?= Just 11
>                    ,  testCase "Question 01-a success spaces left" $
>                           parse parseNumber " 11" @?= Just 11
>                    ,  testCase "Question 01-a success spaces both" $
>                           parse parseNumber "  11  " @?= Just 11
>                    ,  testCase "Question 01-a failure no number" $
>                           parse parseNumber "abc" @?= Nothing
>                    ,  testCase "Question 01-a failure empty" $
>                           parse parseNumber "" @?= Nothing
>                    ]

b) Desenvolva a função

> parsePosition :: Parser Char Position
> parsePosition = f <$> parseNumber <*> parseNumber
>   where
>      f numerox numeroy = (Position numerox numeroy)


que realiza o processamento de uma posição da tartaruga no plano.
Uma posição é representada por um par de números naturais separados
por um ou mais espaços em branco. Sua implementação deve satisfazer
os seguintes casos de teste.



-----------------------Comentario 1 b) ----------------------------------------------------------------------------------
Dada a implementacao do Parse parseNumber a implementacao do ParsePosition fica imediata.
Simplesmente eh chamado sobre uma string de entrada o Parse parseNumber, que ira retornar um numero inteiro
e passara para o outro Parser parseNumber o restante da string a ser processada, que irá resultar no final em dois
inteiros, ambos extraidos de uma mesma string e separados por espaco. Em seguida eh aplicado o fmap 'f' que recebe como
parametro os resultados dos Parsers anteriores e retorna um contrutor Positon passando os dois numeros extraidos.
Ou seja, de um string foi extraido dois numeros inteiros, que por sua vez foram passados como parametro para 
o construtor Position que representa um ponto no plano.




> question01bTests :: TestTree
> question01bTests
>       = testGroup "Question 01-b Tests"
>                    [
>                       testCase "Question 01-b success" $
>                           parse parsePosition "11 22" @?= Just (Position 11 22)
>                    ,  testCase "Question 01-b success spaces right" $
>                           parse parsePosition "11 22  " @?= Just (Position 11 22)
>                    ,  testCase "Question 01-b success spaces left" $
>                           parse parsePosition " 11 22" @?= Just (Position 11 22)
>                    ,  testCase "Question 01-b success spaces both" $
>                           parse parsePosition "  11 22 " @?= Just (Position 11 22)
>                    ,  testCase "Question 01-b failure no number" $
>                           parse parsePosition "abc" @?= Nothing
>                    ,  testCase "Question 01-b failure empty" $
>                           parse parsePosition "" @?= Nothing
>                    ]

Questão 2. Nesta questão desenvolveremos um conjunto de funções para
implementar um parsing do estado inicial da tartaruga no plano que é
representado pelo seguinte tipo.

> data Turtle
>       = Turtle {
>           position :: Position
>         , facing   :: Facing
>         } deriving (Eq, Ord, Show)

O tipo `Facing` denota qual a direção em que a tartaruga está caminhando no plano.

> data Facing = North | South | East | West deriving (Eq, Ord, Show)

2-a) Desenvolva a função

> ehDirecao :: Char -> Bool
> ehDirecao c
>           | c == 'N' = True
>           | c == 'S' = True
>           | c == 'E' = True
>           | c == 'W' = True
>           | otherwise = False

> defineDirecao :: Char -> Facing
> defineDirecao c
>           | c == 'N' = North
>           | c == 'S' = South
>           | c == 'E' = East
>           | c == 'W' = West
>           | otherwise = North


> parseEhDirecao :: Parser Char Facing
> parseEhDirecao = defineDirecao <$> sat ehDirecao


> parseFacing :: Parser Char Facing
> parseFacing = f <$> whitespace <*> parseEhDirecao
>   where
>       f _ direcao = direcao


que realiza o parsing de uma representação da direção da tartaruga. Representaremos
a posição `North` pelo caractere `N`, `South` por `S`, `East` por `E` e `West` por
`W`. Sua função deve satisfazer os seguintes casos de teste:


-----------------------Comentario 2 a) ----------------------------------------------------------------------------------
Para essa questao ser resoolvida foram implementadas as seguintes funcoes:

ehDirecao -> Essa funcao recebe um caractere e retorna Verdadeiro se esse caractere diz respeito a uma direcao entre
as direcoes viaveis: N, S, E , W. Caso contrario retorna Falso. Isso eh feito utilizando casamento de Padrao.

defineDirecao -> Essa funcao recebe um caractere e eh responsavel por retornar o contrutor adequado de Facing
referente aquele caractere. A ideia eh feita por casamento de padrao, bem semelhante a funcao acima (ehDirecao).

parseEhDirecao -> Essa funcao eh um Parser que processa o primeiro caractere da string de entrada caso esse caractere
diga respeito a uma direcao correta. Isso eh feito utilizando sat, e a funcao que vai conferir se uma caractere estah
relacionado a uma direcao ou nao eh a funcao 'ehDirecao'. Caso nao diga respeito a uma direcao 
retornarah uma lista vazia(erro). Em cima do resultado desse sat eh executado sobre o caractere processado a funcao 
defineDirecao que vai converter o caractere em um valor do tipo Facing (direcao). Portanto, foi feito um Parser de uma
string para um Facing

parseFacing -> Essa funcao eh a funcao resposavel por fazer o Parser de uma string para um direcao removendo os espaços.
Basicamente o Parser whitespace eh chamada e entao o parseEhDirecao tambem eh chamado, e uma funcao f eh chamada
sobre o resultado do Parser que nada mais eh que manter o resultado ignorando o processamento dos espacos em branco
pois todo o trabalho de processar um Facing foi feito pelo Parse parseEhDirecao







> question02aTests :: TestTree
> question02aTests
>       = testGroup "Question 02-a Tests"
>                    [
>                       testCase "Question 02-a success" $
>                           parse parseFacing "N" @?= Just North
>                    ,  testCase "Question 02-a success spaces right" $
>                           parse parseFacing "N  " @?= Just North
>                    ,  testCase "Question 02-a success spaces left" $
>                           parse parseFacing " N" @?= Just North
>                    ,  testCase "Question 02-a success spaces both" $
>                           parse parseFacing "  N " @?= Just North
>                    ,  testCase "Question 02-a failure invalid char" $
>                           parse parseFacing "1bc" @?= Nothing
>                    ,  testCase "Question 02-a failure empty" $
>                           parse parseFacing "" @?= Nothing
>                    ]

2-b) Desenvolva a função

> parseTurtle :: Parser Char Turtle
> parseTurtle = f <$> parsePosition <*> parseFacing
>   where
>       f posi faci = Turtle posi faci


que processa o estado inicial de uma tartaruga no plano de execução da linguagem
turtle. Sua função deve atender os seguintes casos de teste:

-----------------------Comentario 2 b) ----------------------------------------------------------------------------------
Apos ser implementado a funcao que faz um Parser de um string para uma Position(Posicao) e o Parser que transforma 
uma string em um Facing(Direcao), entao eh possivel fazer o Parser de uma string para uma Turtle. Esse Parser eh
feito da seguinte forma, primeiro eh extraido a Position utilizando o Parser Positon e com o restante da entrada
eh extraido a Direcao pelo Parser parseFacing. Entao a funcao 'f' que eh um fmap, recebe o resultado dos 2 Parsers e 
tranforma em um contrutor de Turtle.

> tur :: Maybe Turtle
> tur = Just (Turtle (Position 11 22) North)

> question02bTests :: TestTree
> question02bTests
>       = testGroup "Question 02-b Tests"
>                    [
>                       testCase "Question 02-b success" $
>                           parse parseTurtle "11 22 N" @?= tur
>                    ,  testCase "Question 02-b success spaces right" $
>                           parse parseTurtle "11 22 N  " @?= tur
>                    ,  testCase "Question 02-b success spaces left" $
>                           parse parseTurtle " 11 22 N" @?= tur
>                    ,  testCase "Question 02-b success spaces both" $
>                           parse parseTurtle "  11 22 N " @?= tur
>                    ,  testCase "Question 02-b failure invalid char" $
>                           parse parseTurtle "a 11 bc" @?= Nothing
>                    ,  testCase "Question 02-b failure empty" $
>                           parse parseTurtle "" @?= Nothing
>                    ]

Questão 3. O objetivo desta questão é o desenvolvimento de um conjunto de funções para fazer o parsing
de programas da linguagem turtle.

3-a) Instruções da linguagem turtle são representadas pelo seguinte tipo de dados:

> data Instr
>    = Forward | ToLeft | ToRight | Print
>      deriving (Eq, Ord, Show)

Desenvolva a função

> ehInstrucao :: Char -> Bool
> ehInstrucao c
>           | c == 'F' = True
>           | c == 'L' = True
>           | c == 'R' = True
>           | c == 'P' = True
>           | otherwise = False

> defineInstrucao :: Char -> Instr
> defineInstrucao c
>           | c == 'F' = Forward
>           | c == 'L' = ToLeft
>           | c == 'R' = ToRight
>           | c == 'P' = Print
>           | otherwise = Forward


> parseEhInstrucao :: Parser Char Instr
> parseEhInstrucao = defineInstrucao <$> sat ehInstrucao

> parseInstr :: Parser Char Instr
> parseInstr = f <$> whitespace <*> parseEhInstrucao
>   where
>       f _ instrucao = instrucao

que realiza o parsing de uma instrução turtle. Cada instrução é representada por uma letra, como se
segue: `Forward` é representada pela letra `F`, `ToLeft` por `L`, `ToRight` por `R` e `Print` por `P`.
Seu parser deve satisfazer os seguintes casos de teste.


-----------------------Comentario 3 a) ----------------------------------------------------------------------------------
Para essa questao ser resolvida foram implementadas as seguintes funcoes:

ehInstrucao-> Essa funcao recebe um caractere e retorna Verdadeiro se esse caractere diz respeito a uma instrucao entre
as instrucoes viaveis: F, L, R, P. Caso contrario retorna Falso. Isso eh feito utilizando casamento de Padrao.

defineInstrucao -> Essa funcao recebe um caractere e eh responsavel por retornar o contrutor adequado de Instr
referente aquele caractere. A ideia eh feita por casamento de padrao, bem semelhante a funcao acima (ehInstrucao).

parseEhInstrucao -> Essa funcao eh um Parser que processa o primeiro caractere da string de entrada caso esse caractere
diga respeito a uma instrucao correta. Isso eh feito utilizando sat, e a funcao que vai conferir se uma carctere estah
relacionado a uma instrucao ou nao eh a funcao 'ehInstrucao'. Caso nao diga respeito a uma instrucao
retorna uma lista vazia(erro). Em cima do resultado desse sat eh executado sobre o caractere processado a funcao 
defineInstrucao, que vai converter o caractere em um valor do tipo Intr(instrucao). Portanto foi feito um Parser de uma
string para um Intr

parseIntr -> Essa funcao eh a funcao resposavel por fazer o Parser de uma string para uma Instrucao removendo os espaços.
Basicamente o Parser whitespace eh chamado e entao o parseEhInstrucao tambem eh chamado no restante da entrada,
e uma funcao f eh chamada sobre o resultado do Parser que nada mais eh que manter o resultado ignorando o processamento 
dos espacos em branco pois todo o trabalho de processar uma Instr foi feito pelo Parse parseEhInstrucao


> question03aTests :: TestTree
> question03aTests
>       = testGroup "Question 03-a Tests"
>                    [
>                       testCase "Question 03-a success" $
>                           parse parseInstr "F" @?= Just Forward
>                    ,  testCase "Question 03-a success spaces right" $
>                           parse parseInstr "F" @?= Just Forward
>                    ,  testCase "Question 03-a success spaces left" $
>                           parse parseInstr " F" @?= Just Forward
>                    ,  testCase "Question 03-a success spaces both" $
>                           parse parseInstr "  F " @?= Just Forward
>                    ,  testCase "Question 03-a failure invalid char" $
>                           parse parseInstr "a 11 bc" @?= Nothing
>                    ,  testCase "Question 03-a failure empty" $
>                           parse parseInstr "" @?= Nothing
>                    ]

3-b) Programas completos da linguagem turtle são expressos pela configuração inicial
da tartaruga no plano e lista de instruções a serem executadas. O tipo `Program`
representa programas turtle:

> data Program
>       = Program {
>           start :: Turtle
>         , code  :: [Instr]
>         } deriving (Eq, Ord, Show)

O campo `start` representa a posição inicial e code a lista de instruções que deve ser executada.
Com base no apresentado, implemente a função:

> parseProgram :: Parser Char Program
> parseProgram = f <$> parsePosition <*> parseFacing <*> greedy1 parseInstr
>   where 
>       f posi dire instrucoes = (Program (Turtle posi dire) instrucoes)

para realizar o parsing de programas turtle. Seu parser deve atender os seguintes casos de teste.

-----------------------Comentario 3 b) ----------------------------------------------------------------------------------
Apos ser implementado a funcao que faz um Parser de um string para uma Position(Posicao), o Parser que transforma 
uma string em um Facing(Direcao) e o Parser que transforma uma string em uma Instr(Instrucao)
entao eh possivel fazer o Parser de uma string para um Program. Esse Parser eh
feito da seguinte forma, primeiro eh extraido a Position utilizando o Parser Positon, com o restante da entrada
eh extraido a Direcao pelo Parser parseFacing, e por fim sao extraidas as intrucoes do restante da entrada
e sao armazenada em uma lista, utilizando o combinador greedy1, que vai executar varias vezes o parseInstr
colocando os resultados que sao Instr(instrucao) em uma lista, gerando uma lista de instrucoes.
Entao a funcao 'f' que eh um fmap, recebe o resultado dos 3 Parsers e tranforma em um contrutor de Program.

> prog :: Program
> prog = Program (Turtle (Position 11 22) North)
>                [Forward, ToLeft, Forward, Forward]

> question03bTests :: TestTree
> question03bTests
>       = testGroup "Question 03-b Tests"
>                    [
>                       testCase "Question 03-b success" $
>                           parse parseProgram "11 22 N FLFF" @?= Just prog
>                    ,  testCase "Question 03-b success spaces right" $
>                           parse parseProgram "11 22 N FLFF  " @?= Just prog
>                    ,  testCase "Question 03-b success spaces left" $
>                           parse parseProgram " 11 22 N FLFF" @?= Just prog
>                    ,  testCase "Question 03-b success spaces both" $
>                           parse parseProgram "  11 22 N FLFF  " @?= Just prog
>                    ,  testCase "Question 03-b failure invalid char" $
>                           parse parseProgram "b 11 bc" @?= Nothing
>                    ,  testCase "Question 03-b failure empty" $
>                           parse parseProgram "" @?= Nothing
>                    ]

