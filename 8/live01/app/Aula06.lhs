---
author: Programação Funcional
title: Dúvidas e solução de exercícios
date: Prof. Rodrigo Ribeiro
---

Objetivos
=========

- Resolução de alguns exercícios envolvendo listas.

- Dúvidas sobre o conteúdo visto até o momento.

Setup
=====

> module Aula06 where

> import Prelude hiding (take, init, zip, minimum)

> import Test.Tasty
> import Test.Tasty.HUnit

Exercício 1
===========

- Desenvolver a função

> take :: Int -> [a] -> [a]
> take 0 _  = []
> take _ [] = []
> take n (x : xs) = x : take (n - 1) xs

que retorna um prefixo dos "n"
primeiros elementos de uma lista.
Sua implementação deve satisfazer
os seguintes testes:

> takeTests :: TestTree
> takeTests
>   = testGroup "Function take tests"
>       [
>         testCase "take less than list length" $
>           take 3 [1,2,3,4,5] @?= [1,2,3]
>       , testCase "take greater than list length" $
>           take 5 [1,2,3] @?= [1,2,3]
>       , testCase "take equal list length" $
>           take 3 [1,2,3] @?= [1,2,3]
>       ]


Exercício 2
===========

- Desenvolver a função

> init :: [a] -> [a]
> init [] = []
> init [ _ ] = []
> init (x : xs)
>       = x : init xs

que retorna todos os elementos
da lista de entrada, exceto o último.
Sua implementação deve satisfazer
os seguintes testes:

> initTests :: TestTree
> initTests
>   = testGroup "Function init tests"
>       [
>         testCase "init test" $
>            init [1,2,3] @?= [1,2]
>       , testCase "init empty" $
>            init [] @?= ([] :: [Bool])
>       ]

Exercício 3
===========

- Desenvolver a função

> sorted :: [Int] -> Bool
> sorted [] = True
> sorted [ _ ] = True
> sorted (x : x' : xs)
>    = x <= x' && sorted (x' : xs)

que determina se uma lista de inteiros
fornecida como entrada está ou não
ordenada.
Sua implementação deve satisfazer
os seguintes testes:

> sortedTests :: TestTree
> sortedTests
>   = testGroup "Function sorted tests"
>        [
>          testCase "sorted empty"  $ sorted [] @?= True
>        , testCase "sorted single" $ sorted [1] @?= True
>        , testCase "sorted many"   $ sorted [1,2,3] @?= True
>        , testCase "sorted many 1" $ sorted [1,7,4,5] @?= False
>        , testCase "sorted false"  $ sorted [4, 1, 5] @?= False
>        ]


Exercício 4
===========

- Desenvolver a função

> zip :: [a] -> [b] -> [(a,b)]
> zip [] _ = []
> zip _ [] = []
> zip (x : xs) (y : ys)
>       = (x,y) : zip xs ys

que converte duas listas em uma lista
de pares.

Sua implementação deve satisfazer
os seguintes testes:

> zipTests :: TestTree
> zipTests
>   = testGroup "Function zip tests"
>       [
>         testCase "zip list same length" $
>           zip [1,2] [3,4] @?= [(1,3), (2,4)]
>       ,
>         testCase "zip list diff length right" $
>           zip [1,2] [3,4,5] @?= [(1,3), (2,4)]
>       , testCase "zip list diff length right" $
>           zip [1,2,3] [3,4] @?= [(1,3), (2,4)]
>       ]

Exercício 5
===========

- Desenvolver a função

> minimum :: [Int] -> Int
> minimum [] = error "Empty list!"
> minimum [x] = x
> minimum (x : x' : xs) = min x (minimum (x' : xs))

que retorna o maior elemento de uma lista
de números inteiros.

Sua função deve satisfazer os seguintes testes:

> minimumTests :: TestTree
> minimumTests
>       = testGroup "Function mininum tests"
>           [
>              testCase "mininum test" $
>                minimum [1, 0, 10, 5] @?= 0
>           ]


Exercício 6
===========

- Desenvolver a função

> prefixes :: [a] -> [[a]]
> prefixes xs = prefixes' xs []
>   where
>     prefixes' [] ac = [ac]
>     prefixes' (x : xs') ac = ac : prefixes' xs' (ac ++ [x])

prefixes [1,2,3] =
prefixes' [1,2,3] [] =
[] : prefixes' [2,3] ([] ++ [1]) =
[] : prefixes' [2,3] [1] =
[] : ([1] : prefixes' [3] ([1] ++ [2])) =
[] : ([1] : ([1,2] : prefixes' [] ([1,2] ++ [3]))) =
[] : ([1] : [1,2] : [[1,2,3]]) =
[[], [1], [1,2], [1,2,3]]



que retorna todos os prefixos de uma lista
fornecida como entrada.
Sua função deve satisfazer os seguintes testes:

> prefixesTests :: TestTree
> prefixesTests
>    = testGroup "Function prefixes tests"
>          [
>            testCase "prefixes test" $
>               prefixes [1,2,3] @?= [[], [1], [1,2], [1,2,3]]
>          ]


Main
====

> tests :: TestTree
> tests
>  = testGroup "Tests"
>       [
>         takeTests
>       , initTests
>       , sortedTests
>       , zipTests
>       , minimumTests
>       , prefixesTests
>       ]

> main :: IO ()
> main = defaultMain tests



Dúvidas?
========

- Espaço aberto para dúvidas sobre o conteúdo.


Calcular número de elementos


> data Tree a
>   = Leaf
>   | Node a (Tree a) (Tree a)
>   deriving Show

> insert :: Ord a => a -> Tree a -> Tree a
> insert x Leaf = Node x Leaf Leaf
> insert x (Node y l r)
>    = case compare x y of
>         LT  -> Node y (insert x l) r
>         GT  -> Node y l (insert x r)
>         EQ  -> Node y l r

> list2Tree :: Ord a => [a] -> Tree a
> list2Tree [] = Leaf
> list2Tree (x : xs) = insert x (list2Tree xs)

list2Tree [1,2,3] =
insert 1 (list2Tree [2,3]) =
insert 1 (insert 2 (list2Tree [3])) =
insert 1 (insert 2 (insert 3 (list2Tree []))) =
insert 1 (insert 2 (insert 3 Leaf)) =
insert 1 (insert 2 (Node 3 Leaf Leaf)) =
insert 1 (Node 3 (insert 2 Leaf) Leaf) =
insert 1 (Node 3 (Node 2 Leaf Leaf) Leaf) =
Node 3 (insert 1 (Node 2 Leaf Leaf)) Leaf) =
Node 3 (Node 2 (insert 1 Leaf) Leaf) Leaf =
Node 3 (Node 2 (Node 1 Leaf Leaf) Leaf) Leaf

1. Definir o tipo.
2. Definir os casos base.
3. Definir os casos recursivos.

> size :: Tree a -> Int
> size Leaf = 0
> size (Node _ l r) = 1 + nl + nr
>   where
>     nl = size l
>     nr = size r
