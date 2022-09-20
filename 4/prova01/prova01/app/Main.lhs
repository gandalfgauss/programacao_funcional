Avaliação 1 de Programação Funcional
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
$> stack exec prova1-exe
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
atividade "Entrega da Avaliação 1" no Moodle dentro do
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

> import Test.Tasty
> import Test.Tasty.HUnit
> main :: IO ()
> main = defaultMain tests

> tests :: TestTree
> tests
>   = testGroup "Unit tests"
>         [
>            question01Tests
>         ,  question02Tests
>         ,  question03Tests
>         ,  question04Tests
>         ,  question05Tests
>         ]


Questão 1. Escreva a função

> question01 :: [Integer] -> [Integer]
> question01 = question01_aux
>               where
>                   question01_aux [] = []
>                   question01_aux (y:ys)
>                       | even y = y: question01_aux ys
>                       | otherwise = (y*y): question01_aux ys

que recebe uma lista de inteiros como entrada e
retorna como resultado uma lista de inteiros em que
todo número ímpar presente na lista é elevado ao quadrado.
Sua implementação deve atender os seguintes casos de teste.




> question01Tests :: TestTree
> question01Tests
>       = testGroup "Question 01 Tests"
>                    [
>                       testCase "Question 01 empty" $
>                           question01 [] @?= []
>                    ,  testCase "Question 01 all even" $
>                           question01 (map (* 2) [1..5]) @?= map (* 2) [1..5]
>                    ,  testCase "Question 01 some odd" $
>                           question01 [1..5] @?= [1,2,9,4,25]
>                    ]


Comentario Questão 1
A questão acima recebe uma Lista de Inteiros e retorna a mesma lista,
porem com os numeros impares ao quadrado. O caso base eh, 
se a lista for vazia entao eh retornado uma lista vazia.
Se a lista for nao vazia entao eh conferido se a cabeca da lista
eh par atarves da funcao even, se for entao a cabeca eh inserida na frente da chamada recursiva do corpo da lista
caso contrario a cabeca eh impar, entao o quadrado da cabeca eh inserido na frente
da chamada recursiva do corpo da lista


Questão 2. Considere o seguinte tipo de dados:

> data Times = Zero | One | Two

Sua tarefa é implementar a função:

> question02 :: Times -> (a, a, a) -> (a, a, a)
> question02 = question02_aux
>               where 
>                   question02_aux Zero (x2, y2, z2) = (x2, y2, z2)
>                   question02_aux One (x2, y2, z2) = (z2, x2, y2)
>                   question02_aux Two (x2, y2, z2) = (y2, z2, x2)


> question02Tests :: TestTree
> question02Tests
>       = testGroup "Question 02 Tests"
>                   [
>                      testCase "Swapping Zero times:" $
>                           question02 Zero ("a","b","c") @?= ("a","b","c")
>                   ,  testCase "Swapping One time:" $
>                           question02 One ("a", "b", "c") @?= ("c", "a", "b")
>                   ,  testCase "Swapping Two times:" $
>                           question02 Two ("a", "b", "c") @?= ("b", "c", "a")
>                   ]


Comentario Questão 2
A questao 2 consiste em rotacionar os elementos de uma tripla
baseado no valor do Tipo times, como foi feito acima
se o valor do tipo times for Zero entao a propria tripla eh retornada
pois nao deve ser feita nenhuma rotacao, mas se times for One
entao a tripla retornada deve ser na seguinte posicao:
(3,1,2), considerando que a tripla na posicao inicial eh (1,2,3)
e finalmente, se times valer Two entao deve ser retornada um tripla
na posicao (2,3,1). Tudo isso eh feito por casamento de padrao.


que a partir de um valor do tipo `Times` e uma tripla
de valores de tipo `a`, retorna uma tripla na qual os
valores foram rotacionados um número de vezes especificado
pelo tipo `Times.` Os casos de teste a seguir apresentam
exemplos desta função.

Questão 03. Considere o seguinte tipo de dados que representa
dados de clientes de uma loja:

> type Name  = String
> type Phone = String
> type Email = String

> data Client = Client Name Phone Email deriving (Eq, Show)


Dizemos que a informação de um cliente é válida se:

a) O nome do cliente possui pelo menos 3 caracteres e é
formado exclusivamente por letras e espaços.

b) A informação de telefone é composta apenas por dígitos

c) A string de email deve conter o caractere `@` e ter tamanho
maior que 3.

Com base nessas informações, desenvolva a função:

> question03 :: Client -> Bool
> question03 = question03_aux
>               where
>                   question03_aux (Client nome phone email) = nomeValido nome && telefoneValido phone && emailValido email
>                       
>                   nomeValido nameClient
>                       | length nameClient > 2 = nomeValido2 nameClient
>                       | otherwise = False
>                   
>                   nomeValido2 [] = True
>                   nomeValido2 (caractere:nome) = nomeValido3 caractere "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ " && nomeValido2 nome
>                   
>                   nomeValido3 _ [] = False
>                   nomeValido3 caracter (c:cs)
>                       | caracter == c = True
>                       | otherwise = nomeValido3 caracter cs
>
>                   telefoneValido [] = True
>                   telefoneValido (f:fs) = telefoneValido2 f "1234567890" && telefoneValido fs
>
>                   telefoneValido2 _ [] = False
>                   telefoneValido2 caractereTelefone (caractereFone: telefon)
>                       | caractereTelefone == caractereFone = True
>                       | otherwise = telefoneValido2 caractereTelefone telefon
>
>                   emailValido email
>                       | length email > 3 = emailValido2 email
>                       | otherwise = False
>
>                   emailValido2 [] = False
>                   emailValido2 (ex:email_xs)
>                       | ex == '@' = True
>                       | otherwise = emailValido2 email_xs
>
>                   


que verifica se a informação de cliente é ou não válida de
acordo com as regras mencionadas anteriormente.

Sua implementação deve considerar os seguintes casos de teste.

> question03Tests :: TestTree
> question03Tests
>       = testGroup "Question 03 Tests"
>                   [
>                      testCase "Valid client" $
>                        question03 (Client "Marcos" "123456789" "marcos@bla.com") @?= True
>                   ,  testCase "Invalid name - size" $
>                        question03 (Client "Mr" "123456789" "marcos@bla.com") @?= False
>                   ,  testCase "Invalid name - not all letters" $
>                        question03 (Client "Mr22" "123456789" "marcos@bla.com") @?= False
>                   ,  testCase "Invalid phone" $
>                        question03 (Client "Marcos" "ab23" "marcos@bla.com") @?= False
>                   ,  testCase "Invalid email - size" $
>                        question03 (Client "Marcos" "123456789" "m@") @?= False
>                   ,  testCase "Invalid email - lacking @" $
>                        question03 (Client "Marcos" "123456789" "marcobla.com") @?= False
>                   ]



Comentario Questão 3
A questao 3 deve verificar se as informacoes de um cliente sao validas,
o cliente eh composto por nome, telefone e email
Entao as informacoes sao validas somente se o nome eh valido, e o telefone
eh valido e o email eh valido.
Conferindo nome:
para conferir se o nome eh valido entao a funcao nomeValido eh chamada
Se o tamanho do nome for maior do que 2, ou seja, 3 adiante, entao eh chamada
a funcao nomeValido2, caso contrario retorna False.
A funcao nomeValido2 tem como objetivo conferir se os caracteres presentes
no nome do cliente sao somente letras e espaços. Em seu caso base retorna True
para um nome vazio, e para cada letra do nome, a funcao nomeValido3 eh chamada
no intuito de conferir se um caractere do nome eh uma letra ou um espaco.
A funcao nomeValido3 recebe um caractere e uma lista de letras e um espaco, e para cada
caractere valido eh conferido se o caractere do nome eh igual ao mesmo, se for retorna True,
caso contrario a funcao nomeValido3 eh chamada recursivamente para o restante dos caracteres validos.
Se chegar ao final da lista quer dizer que o caractere passado como parâmetro nao eh
nenhum caractere valido, entao eh retornado False.

A funcao telefoneValido recebe um telefone (String) e retorna True se o telefone eh 
valido, False caso contrario. Se nao tiver telefone retorna True, pois o problema
nao disse se o cliente tem que ter um telefone cadastrado. A ideia dessa funcao
eh percorrer desde o primeiro digito de telefone(cabeca da lista) ate o final chamando
a funcao telefoneValido2 (mesma ideia do nomeValido3) , no intuito de fazer o "e" logico entre
as chamadas de maneira que eh retornado True somente se cada caractere do telefone for um numero.

A funcao emailValido, recebe um email e confere se o mesmo eh valido.
Se o email tiver tamanho maior que 3(utilizando a funcao length para descobrir o tamanho do email),
entao eh chamada a funcao emailValido2, caso contrario retorna False.

A funcao emailValido2 recebe um email e percorre todos os caracteres do email(da cabeca ate o final),
no intuito de encontrar o caractere '@' no email. Se encontrar retorna True, caso contrario eh conferido 
se o proximo caractere do email eh o '@', se chegar ao final do email(lista vazia) que dizer
que o caracter '@' nao foi encontrado então retorna False.




Questão 04. Um inconveniente da solução apresentada no exercício 03 é que a função não
apresenta uma explicação do motivo da validação falhar. Uma alternativa para isso é
criar um tipo de dados para representar as possíveis falhas de validação.

> data Error = NameLengthError       -- invalid size
>            | NameCharactersError   -- name with non-letters and space characters
>            | PhoneError            -- phone with non numeric chars.
>            | EmailSizeError        -- invalid size
>            | EmailCharError        -- lacking `@`
>            deriving (Eq, Show)


Usando a representação de erros de validação, podemos definir um tipo para representar
a validação:

> data Validation = Ok
>                 | Failure [Error] deriving (Eq, Show)

O construtor `Ok` representa que a validação executou com sucesso e o construtor `Failure`
representa uma falha de validação e armazena uma lista dos erros encontrados.

Com base no apresentado, implemente a função.

> question04 :: Client -> Validation
> question04 = question04_aux 
>               where
>                   question04_aux (Client nome phone email) = question04_aux2  [nameLenght nome, nomeCharacters nome, phoneValid phone, emailSize email, emailChar email] [NameLengthError, NameCharactersError, PhoneError, EmailSizeError, EmailCharError]
>                                                               
>                                                        
>                   question04_aux2 xs ys
>                       | question04_aux3 xs ys == (Failure []) = Ok
>                       | otherwise = question04_aux3 xs ys
>      
>                   question04_aux3 xs ys = Failure ([y | (x,y) <- zip xs ys, not x])
>                     
>                   nameLenght nameClient
>                       | length nameClient > 2 = True
>                       | otherwise = False
>                   
>                   nomeCharacters [] = True
>                   nomeCharacters (caractere:nome) = nomeCharacters2 caractere "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ " && nomeCharacters nome
>                   
>                   nomeCharacters2 _ [] = False
>                   nomeCharacters2 caracter (c:cs)
>                       | caracter == c = True
>                       | otherwise = nomeCharacters2 caracter cs
>
>                   phoneValid [] = True
>                   phoneValid (f:fs) =  phoneValid2 f "1234567890" &&  phoneValid fs
>
>                   phoneValid2 _ [] = False
>                   phoneValid2 caractereTelefone (caractereFone: telefon)
>                       | caractereTelefone == caractereFone = True
>                       | otherwise =  phoneValid2 caractereTelefone telefon
>
>                   emailSize email
>                       | length email > 3 = True
>                       | otherwise = False
>
>                   emailChar [] = False
>                   emailChar (ex:email_xs)
>                       | ex == '@' = True
>                       | otherwise = emailChar email_xs
>
>      

que realiza a validação de clientes, como apresentado na questão 03, e retorna um valor do
tipo `Validation`. Sua implementação deve atender os seguintes casos de teste.

> question04Tests :: TestTree
> question04Tests
>       = testGroup "Question 04 Tests"
>                   [
>                      testCase "Valid client" $
>                        question04 (Client "Marcos" "123456789" "marcos@bla.com") @?= Ok
>                   ,  testCase "Invalid name - size" $
>                        question04 (Client "Mr" "123456789" "marcos@bla.com") @?= Failure [NameLengthError]
>                   ,  testCase "Invalid name - not all letters" $
>                        question04 (Client "Mr22" "123456789" "marcos@bla.com") @?= Failure [NameCharactersError]
>                   ,  testCase "Invalid phone" $
>                        question04 (Client "Marcos" "ab23" "marcos@bla.com") @?= Failure [PhoneError]
>                   ,  testCase "Invalid email - size" $
>                        question04 (Client "Marcos" "123456789" "m@") @?= Failure [EmailSizeError]
>                   ,  testCase "Invalid email - lacking @" $
>                        question04 (Client "Marcos" "123456789" "marcobla.com") @?= Failure [EmailCharError]
>                   ,  testCase "Combining errors" $
>                        question04 (Client "Mr" "aa" "b@") @?= Failure [NameLengthError, PhoneError, EmailSizeError]
>                   ]

Comentario questão 4

A funcao question04 recebe um cliente e retorna Ok se todos os seus dados estiverem corretos e caso contrario
sera retornado uma Lista de Erros. A funcao question04 chama a funcao question04_aux passando como parametro
o cliente a ser analisado e a funcao question04_aux passa chama a funcao question04_aux2 passando como parametro
duas lista, a primeira lista eh uma lista de valores booleanos, e esses valores sao verdadeiro ou falso
dependendo se o tipo de validacao sobre o nome, telefone ou email esta correta  ou nao, ou seja,
se o nome do cliente tiver menos que 3 caracteres entao True eh colocado na lista na primeira posicao,
e assim por diante, as funcoes que fazem as validacoes ja foram implementadas na Questao03 e foi
descrito o passo a passo do funcionamento das mesmas, elas sofreram umas leves modificacoes para
ficarem mais modulares, mas o contexto eh o mesmo. A segunda lista passa como parametro para a funcao question04_aux2 
eh a lista dos possiveis erros que podem apararecer.
A funcao question04_aux2 tem como objetivo retornar Ok se nao tiver problema na validacao do Cliente,
ou seja, a primeira lista eh toda True, e retornar um Failure caso contrario. Para isso
eh utilizado a funcao question04_aux3 que zipa as duas lista de retorna uma lista
atraves de um list comprehension, e nessa nova lista contem somente os erros cujo seu par na tupla iterada tenha 
o valor False. Ou seja é retornado os erros que possuem False dentro da tupla. Em seguida a funcao question04_aux2
confere se essa lista de erros esta vazia, se estiver retorna Ok, caso contrario retorna essa lista de Erros,
com o contrutor Failure

Questão 05. Considere o seguinte tipo de dados que representa a configuração de uma
aplicação em um sistema gerenciador de janelas:

> data App
>       = App { name :: String  -- application name
>             , width :: Int    -- window width
>             , height :: Int   -- window height
>             }
>         deriving (Eq, Show)


Aplicações são organizadas de acordo com um layout:

> data Layout = Vertical [Layout]
>             | Horizontal [Layout]
>             | Single App
>             deriving (Eq, Show)

Neste gerenciador de janelas simples, aplicações são organizadas de maneira vertical
(construtor `Vertical`), horizontal (construtor `Horizontal`) ou uma janela simples.

Seu objetivo é implementar a função:

> minimizeAll :: Layout -> Layout
> minimizeAll = minimizeAll_aux 
>                   where
>                       minimizeAll_aux (Single (App name_aux _ _)) = (Single (App name_aux 1 1))
>                       minimizeAll_aux (Vertical layoutv) = (Vertical [minimizeAll_aux lv | lv <- layoutv])
>                       minimizeAll_aux (Horizontal layouth)= (Horizontal [minimizeAll_aux lh | lh <- layouth])
>                       

que minimiza todas as janelas do estado do gerenciador de janelas. Uma janela é
minimizada fazendo com que sua altura (height) e comprimento (width) sejam iguais a 1.

Sua implementação deve atender os seguintes casos de teste.

> question05Tests :: TestTree
> question05Tests
>       = testGroup "Question 05 Tests"
>                   [
>                       testCase "Minimize Single" $
>                         minimizeAll (Single (App "test" 110 200)) @?= Single (App "test" 1 1)
>                   ,   testCase "Minimize Vertical" $
>                         minimizeAll (Vertical [ Single (App "test" 110 200)
>                                               , Horizontal [Single (App "foo" 300 100)]])
>                               @?= Vertical [ Single (App "test" 1 1)
>                                            , Horizontal [Single (App "foo" 1 1)]]
>                   ,   testCase "Minimize Horizontal" $
>                         minimizeAll (Horizontal [ Single (App "test" 110 200)
>                                                 ,   Vertical [Single (App "foo" 300 100)]])
>                               @?= Horizontal [ Single (App "test" 1 1)
>                                              , Vertical [Single (App "foo" 1 1)]]
>                   ]


Comentario Questao 05
A funcao minimizeAll recebe um Layout e tem que minimizar todas as janelas desse Layout
Para isso ela chama a funcao minimizeAll_aux que tambem recebe o Layout a ser minimizado
e retorna o Layout minimizado.
Nos casamentos de padrao:
Se for encontrado o construtor Single, entao basta retornar um novo contrutor com o mesmo nome
porem com tamanhos alterado para 1.
Se for encontrado os contrutores Vertical ou Horizontal, basta retornar o mesmo contrutor,
porem fazendo uma chamada recursiva da funcao minimizeAll para cada elemento da lista de layout,
para isso foi utilizado um list comprehension, que ira retornar uma lista de layouts minimizados.