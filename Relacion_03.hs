-- I1M: Relacion_03.hs
-- Definiciones por comprensión (I)
-- Departamento de Ciencias de la Computación e Inteligencia Artificial
-- Universidad de Sevilla
-- ============================================================================


-- ============================================================================
-- Librerías auxiliares
-- ============================================================================

import Test.QuickCheck
import Data.Numbers.Primes
import Data.List 

-- ============================================================================
-- Operaciones con conjuntos
-- ============================================================================

-- ----------------------------------------------------------------------------
-- Un conjunto se puede representar como una lista teniendo en cuenta que el
-- orden entre sus elementos no es importante. Resolver los siguientes
-- ejercicios utilizando esta representación.
-- ----------------------------------------------------------------------------

-- ----------------------------------------------------------------------------
-- Ejercicio 1. Definir por comprensión la función
--   union :: Eq a => [a] -> [a] -> [a]
-- tal que '(union xs ys)' es la unión de los conjuntos 'xs' e 'ys'. Por
-- ejemplo,
--   union [3,2,5] [5,7,3,4]  ==  [3,2,5,7,4]
--   union [1,3,5,7] [2,4,6]  ==  [1,3,5,7,2,4,6]
-- ----------------------------------------------------------------------------

union :: Eq a => [a] -> [a] -> [a]
union xs ys = nub(xs ++ ys)
-- ----------------------------------------------------------------------------
-- Ejercicio 2. Definir por comprensión la función
--   interseccion :: Eq a => [a] -> [a] -> [a]
-- tal que '(interseccion xs ys)' es la intersección de los conjuntos 'xs' e
-- 'ys'. Por ejemplo,
--   interseccion [3,2,5] [5,7,3,4]  ==  [3,5]
--   interseccion [3,2,5] [9,7,6,4]  ==  []
-- ----------------------------------------------------------------------------

interseccion :: Eq a => [a] -> [a] -> [a]
interseccion [] _ = []
interseccion _ [] = []
interseccion xs ys = [x | x<-xs, elem x ys == True]
  
-- ----------------------------------------------------------------------------
-- Ejercicio 3. Definir por comprensión la función
--   diferencia :: Eq a => [a] -> [a] -> [a]
-- tal que '(diferencia xs ys)' es la diferencia entre los conjuntos 'xs' e
-- 'ys'. Por ejemplo,
--   diferencia [3,2,5,6] [5,7,3,4]  ==  [2,6]
--   diferencia [3,2,5] [5,7,3,2]    ==  []
-- ----------------------------------------------------------------------------

diferencia :: Eq a => [a] -> [a] -> [a]
diferencia [] _ = []
diferencia xs [] = xs
diferencia xs ys = [x | x<-xs, elem x ys == False]

-- ----------------------------------------------------------------------------
-- Ejercicio 4. Definir por comprensión la función
--   subconjunto :: Eq a => [a] -> [a] -> Bool
-- tal que '(subconjunto xs ys)' se verifica si el conjunto 'xs' es un
-- subconjunto del conjunto 'ys'. Por ejemplo,
--   subconjunto [3,2,3] [2,5,3,5]  ==  True
--   subconjunto [3,2,3] [2,5,6,5]  ==  False
-- ----------------------------------------------------------------------------

subconjunto :: Eq a => [a] -> [a] -> Bool
subconjunto [] _ = True
subconjunto _ [] = True
subconjunto xs ys | [x | x<-xs, elem x ys == True] == xs = True
                  | otherwise = False
                  
-- ----------------------------------------------------------------------------
-- Ejercicio 5. Definir por comprensión la función
--   iguales :: Eq a => [a] -> [a] -> Bool
-- tal que '(iguales xs ys)' se verifica si los conjuntos 'xs' e 'ys' son
-- iguales. Por ejemplo,
--   iguales [3,2,3] [2,3]    ==  True
--   iguales [3,2,3] [2,3,2]  ==  True
--   iguales [3,2,3] [2,3,4]  ==  False
-- ---------------------------------------------------------------------------

iguales :: Eq a => [a] -> [a] -> Bool
iguales _ [] = True
iguales [] _ = True
iguales xs ys | subconjunto xs ys == True && subconjunto ys xs == True = True
              |otherwise = False

-- ----------------------------------------------------------------------------
-- Nota. En los ejercicios de comprobación de propiedades, cuando se trata con
-- igualdades se usa la igualdad de conjuntos (definida por la función
-- 'iguales') en lugar de la igualdad de listas (definida por la función '==')
-- ----------------------------------------------------------------------------

-- ----------------------------------------------------------------------------
-- Ejercicio 6. Comprobar con QuickCheck que la unión de conjuntos es
-- conmutativa.
-- ----------------------------------------------------------------------------

prop_union_conmutativa :: [Int] -> [Int] -> Bool
prop_union_conmutativa xs ys = undefined

-- La comprobación es
--   > quickCheck prop_union_conmutativa

-- ----------------------------------------------------------------------------
-- Ejercicio 7. Comprobar con QuickCheck si se cumple la siguiente propiedad:
--   A ∪ (B ∩ C) = (A ∪ B) ∩ C
-- En caso de que no se cumpla comprobar el contraejemplo obtenido con
-- QuickCheck.
-- ----------------------------------------------------------------------------

prop_union_interseccion :: [Int] -> [Int] -> [Int] -> Bool
prop_union_interseccion xs ys zs = undefined

-- La comprobación es
--   > quickCheck prop_union_interseccion

-- ----------------------------------------------------------------------------
-- Ejercicio 8. Comprobar con QuickCheck si la diferencia de conjuntos es
-- conmutativa. En caso de que no se cumpla comprobar el contraejemplo
-- obtenido con QuickCheck.
-- ----------------------------------------------------------------------------

prop_diferencia_conmutativa :: [Int] -> [Int] -> Bool
prop_diferencia_conmutativa xs ys = undefined

-- La comprobación es
--   > quickCheck prop_diferencia_conmutativa

-- ----------------------------------------------------------------------------
-- Ejercicio 9. Comprobar con QuickCheck si se cumple la siguiente propiedad:
--   A \ B ⊂ A
-- En caso de que no se cumpla comprobar el contraejemplo obtenido con
-- QuickCheck.
-- ----------------------------------------------------------------------------

prop_diferencia_subconjunto :: [Int] -> [Int] -> Bool
prop_diferencia_subconjunto xs ys = undefined

-- La comprobación es
--   > quickCheck prop_diferencia_subconjunto

-- ----------------------------------------------------------------------------
-- Ejercicio 10. Comprobar con QuickCheck si se cumple la siguiente propiedad:
--   (A \ B) ∩ B = ∅.
-- En caso de que no se cumpla comprobar el contraejemplo obtenido con
-- QuickCheck.
-- ----------------------------------------------------------------------------

prop_diferencia_interseccion :: [Int] -> [Int] -> Bool
prop_diferencia_interseccion xs ys = undefined

-- La comprobación es
--   > quickCheck prop_diferencia_interseccion

-- ============================================================================
-- Factores primos y divisores
-- ============================================================================

-- ----------------------------------------------------------------------------
-- Ejercicio 11. Definir por comprensión la función
--   divisores :: Int -> [Int]
-- tal que '(divisores n)' es la lista de los divisores del número 'n'. Por
-- ejemplo,
--   divisores 30  ==  [1,2,3,5,6,10,15,30]
-- ----------------------------------------------------------------------------

divisores :: Int -> [Int]
divisores n | isPrime n = [1,n]
            | otherwise = [i | i <-[1..(n `div` 2)], n `mod` i == 0] ++ [n]

-- ----------------------------------------------------------------------------
-- Ejercicio 12. Definir por comprensión la función
--   primo :: Int -> Bool
-- tal que '(primo n)' se verifica si 'n' es un número primo. Por ejemplo,
--   primo 30  ==  False
--   primo 15  ==  False
--   primo 17  ==  True
-- ----------------------------------------------------------------------------

primo :: Int -> Bool
primo n = isPrime n

-- ----------------------------------------------------------------------------
-- Ejercicio 13. Definir por comprensión la función
--   divisoresPrimos :: Int -> [Int]
-- tal que '(divisoresPrimos n)' es la lista de los divisores primos del número
-- 'n'. Por ejemplo,
--   divisoresPrimos 40  ==  [2,5]
--   divisoresPrimos 70  ==  [2,5,7]
-- ----------------------------------------------------------------------------

divisoresPrimos :: Int -> [Int]
divisoresPrimos n = filter isPrime (divisores n)

-- ----------------------------------------------------------------------------
-- Ejercicio 14. Un número es libre de cuadrados si no es divisible por el
-- cuadrado de ningún entero mayor que 1. Por ejemplo, 70 es libre de cuadrados
-- porque sólo es divisible por 1, 2, 5, 7 y 70; en cambio, 40 no es libre de
-- cuadrados porque es divisible por 2².
--
-- Definir por comprensión la función
--   libreDeCuadrados :: Int -> Bool
-- tal que '(libreDeCuadrados n)' se verifica si el número 'n' es libre de
-- cuadrados. Por ejemplo,
--   libreDeCuadrados 70  ==  True
--   libreDeCuadrados 40  ==  False
-- ----------------------------------------------------------------------------

libreDeCuadrados :: Int -> Bool
libreDeCuadrados n | interseccion [i^2 | i<-[2..(n `div` 2)]] (divisores n) == [] = True
                   | otherwise = False

-- ----------------------------------------------------------------------------
-- Ejercicio 15. Un entero positivo es perfecto si es igual a la suma de sus
-- divisores propios, excluyendo el propio número.
--
-- Definir por comprensión la función
--   perfectos :: Int -> [Int]
-- tal que '(perfectos n)' es la lista de todos los números perfectos menores
-- o iguales que el número 'n'. Por ejemplo,
--   perfectos 500  ==  [6,28,496]
-- ----------------------------------------------------------------------------

esPerfecto :: Int -> Bool
esPerfecto n =
  if sum(divisores n) == 2*n
  then True
  else False

perfectos :: Int -> [Int]
perfectos n = filter esPerfecto [6..n]

-- ----------------------------------------------------------------------------
-- Ejercicio 16. Un entero positivo se denomina abundante si es menor que la
-- suma de sus divisores propios. Por ejemplo, 12 y 30 son abundantes pero 5 y
-- 28 no lo son.
--
-- Definir por comprensión la función
--   abundante :: Int -> Bool
-- tal que '(abundante n)' se verifica si 'n' es un número abundante. Por
-- ejemplo,
--   abundante 5   ==  False
--   abundante 12  ==  True
--   abundante 28  ==  False
--   abundante 30  ==  True
-- ----------------------------------------------------------------------------

abundante :: Int -> Bool
abundante n =
  if k > n
  then True
  else False
       where k = sum [i | i<-[2..(n `div` 2)], n `mod` i == 0] + 1

-- ----------------------------------------------------------------------------
-- Ejercicio 17. La multiplicidad de un número en otro es la mayor potencia del
-- primero que divide al segundo. Por ejemplo, la multiplicidad de 2 en 40 es 3
-- porque 40 es divisible por 2³ y no lo es por 2⁴. Además, la multiplicidad
-- de 1 en cualquier número se supone igual a 1.
--
-- Definir por comprensión la función
--   multiplicidad :: Int -> Int -> Int
-- tal que '(multiplicidad n m)' es la multiplicidad de 'n' en 'm'. Por
-- ejemplo,
--   multiplicidad 2 40  ==  3
--   multiplicidad 5 40  ==  1
--   multiplicidad 3 40  ==  0
--   multiplicidad 1 40  ==  1
-- ----------------------------------------------------------------------------
{-
mult :: Int -> Int -> Int -> Int
mult n 0 m = 0
mult n k m |m `mod` (n^k) == 0 = k
           |otherwise = mult n (k-1) m

multiplicidad :: Int -> Int -> Int
multiplicidad 1 m = 1
multiplicidad n m = mult n k m
                      where k = floor (logBase n m)
-}
--maximum[n^i | i<-[1..(ceiling ((log m)/(log n)))], m `mod` (n^i) == 0]
--maximum(interseccion [n^i | i<-[1..], (n^i) <= m] (divisores m))
-- ----------------------------------------------------------------------------
-- Ejercicio 18. Dado un entero positivo n, el reducido de n es el número que
-- tiene los mismos factores primos que n pero con su multiplicidad disminuida
-- en uno. Por ejemplo,
--   r(8) = 4, pues 8 = 2³
--   r(50) = 5, pues 50 = 2*5²
--   r(100) = 10, pues 100 = 2²*5²
--   r(58800) = 280, pues 58800 = 2⁴*3*5²*7²
--
-- Definir por comprensión la función
--   reducido :: Int -> Int
-- tal que '(reducido n)' es el reducido del número 'n'. Por ejemplo,
--   reducido 8      ==  4
--   reducido 50     ==  5
--   reducido 100    ==  10
--   reducido 58800  ==  280
-- ----------------------------------------------------------------------------

reducido :: Int -> Int
reducido n = n `div` product(nub(primeFactors n))

-- ----------------------------------------------------------------------------
-- Ejercicio 19. [Problema 21 del Proyecto Euler] Sea d(n) la suma de los
-- divisores propios de n. Si d(a) = b y d(b) = a, siendo a ≠ b, decimos que a
-- y b son un par de números amigos. Por ejemplo, los divisores propios de 220
-- son 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 y 110; por tanto, d(220) = 284. Los
-- divisores propios de 284 son 1, 2, 4, 71 y 142; por tanto, d(284) = 220.
-- Luego, 220 y 284 son dos números amigos.
--
-- Definir por comprensión la función
--   amigos :: Int -> Int -> Bool
-- tal que '(amigos n m)' se verifica si 'n' y 'm' son números amigos. Por
-- ejemplo,
--   amigos 6 6        ==  False
--   amigos 220 248    ==  False
--   amigos 220 284    ==  True
--   amigos 100 200    ==  False
--   amigos 1184 1210  ==  True
-- ----------------------------------------------------------------------------

amigos :: Int -> Int -> Bool
amigos n m |n==m = False
           |sum(divisores n) - n == sum(divisores m) - m = True
           |otherwise = False

-- ----------------------------------------------------------------------------
-- Ejercicio 20. [Problema 211 del proyecto Euler] Dado un entero positivo n,
-- consideremos la suma de los cuadrados de sus divisores, Por ejemplo,
--   f(10) = 1 + 4 + 25 + 100 = 130
--   f(42) = 1 + 4 +  9 +  36 + 49 + 196 + 441 + 1764 = 2500
-- Decimos que n es especial si f(n) es un cuadrado perfecto. En los ejemplos
-- anteriores, 42 es especial y 10 no lo es.
--
-- Definir por comprensión la función
--   especial :: Int -> Bool
-- tal que '(especial n)' se verifica si 'n' es un número especial. Por
-- ejemplo,
--   especial 42  ==  True
--   especial 10  ==  False
-- ----------------------------------------------------------------------------


{-
especial :: Int -> Bool
especial n = elem (sqrt (sum(map (^2) (divisores n)))) [1..(10^10)] == True = True
-}
-- ============================================================================
-- Combinación de generadores
-- ============================================================================

-- ----------------------------------------------------------------------------
-- Ejercicio 21. Definir por comprensión la función
--   conPos :: [a] -> [(a,Int)]
-- tal que '(conPos xs)' es la lista obtenida a partir de 'xs' especificando
-- las posiciones de sus elementos. Por ejemplo,
--   conPos [1,5,0,7]  ==  [(1,0),(5,1),(0,2),(7,3)]
-- ----------------------------------------------------------------------------

conPos :: [a] -> [(a,Int)]
conPos xs = zip xs [1..(length xs)]

-- ----------------------------------------------------------------------------
-- Ejercicio 22. Una representación de 20 en base 2 es [0,0,1,0,1] pues
-- 20 = 0*2⁰ + 0*2¹ + 1*2² + 0*2³ + 1*2⁴. Y una representación de 46 en
-- base 3 es [1,0,2,1] pues 46 = 1*3⁰ + 0*3¹ + 2*3² + 1*3³.
--
-- Definir por comprensión la función
--   deBaseABase10 :: Int -> [Int] -> Int
-- tal que '(deBaseABase10 b xs)' es el número 'n' tal que su representación
-- en base 'b' es 'xs'. Por ejemplo,
--   deBaseABase10 2 [0,0,1,0,1]      ==  20
--   deBaseABase10 2 [1,1,0,1]        ==  11
--   deBaseABase10 3 [1,0,2,1]        ==  46
--   deBaseABase10 5 [0,2,1,3,1,4,1]  ==  29160
-- ----------------------------------------------------------------------------

deBaseABase10 :: Int -> [Int] -> Int
deBaseABase10 b xs = 

-- ----------------------------------------------------------------------------
-- Ejercicio 23. Dos listas xs, ys de la misma longitud son perpendiculares si
-- el producto escalar de ambas es 0, donde el producto escalar de dos listas
-- de enteros xs e ys viene dado por la suma de los productos de los elementos
-- correspondientes.
--
-- Definir por comprensión la función
--   perpendiculares :: (Num a, Eq a) => [a] -> [[a]] -> [[a]]
-- tal que '(perpendiculares xs yss)' es la lista de los elementos de 'yss' que
-- son perpendiculares a 'xs'. Por ejemplo,
--   perpendiculares [1,0,1] [[0,1,0],[2,3,1],[-1,7,1],[3,1,0]]  ==
--     [[0,1,0],[-1,7,1]]
-- ----------------------------------------------------------------------------
esPerpen :: (Num a, Eq a) => [a] -> [a] -> Bool
esPerpen [] [] = True
esPerpen [] _ = False
esPerpen _ [] = False
esPerpen xs xy | length xs /= length xy = False
               | sumaComp xs xy == 0 = True
               | otherwise = False

sumaComp :: (Num a, Eq a) => [a] -> [a] -> a
sumaComp [] [] = 0
sumaComp (x:xs) (y:ys) = x*y+sumaComp xs ys


perpendiculares :: (Num a, Eq a) => [a] -> [[a]] -> [[a]]
perpendiculares xs yss = [ys | ys<-yss, esPerpen xs ys == True]

-- ----------------------------------------------------------------------------
-- Ejercicio 24. La distancia entre dos números es el valor absoluto de su
-- diferencia. Por ejemplo, la distancia entre 2 y 5 es 3.
--
-- Definir por comprensión la función
--   cercanos :: [Int] -> [Int] -> [(Int,Int)]
-- tal que '(cercanos xs ys)' es la lista de pares de elementos de 'xs' e 'ys'
-- cuya distancia es mínima. Por ejemplo,
--   cercanos [3,7,2,1] [5,11,9]  ==  [(3,5),(7,5),(7,9)]
-- ----------------------------------------------------------------------------
{-prodEsc :: [Int] -> [Int] -> [Int]
prodEsc [] _ = []
prodEsc _ [] = []
prodEsc (x:xs) ys = [abs (x-y) | y<-ys] ++ prodEsc xs ys

cercanos :: [Int] -> [Int] -> [(Int,Int)]
cercanos xs ys = undefined
-}
cercanos xs ys =
  let d = minimum [abs]

-- ----------------------------------------------------------------------------
-- Ejercicio 25. Dada una lista de números enteros, definiremos el mayor salto
-- como la mayor distancia entre números consecutivos de la lista. Por ejemplo,
-- dada la lista [2,5,-3], las distancias entre números consecutivos de la
-- lista son:
--   3 (valor absoluto de la diferencia entre 2 y 5) y
--   8 (valor absoluto de la diferencia entre 5 y (-3))
-- Por tanto, su mayor salto es 8. No está definido el mayor salto para listas
-- con menos de 2 elementos.
--
-- Definir por comprensión la función
--   mayorSalto :: [Int] -> Int
-- tal que '(mayorSalto xs)' es el mayor salto de la lista 'xs'. Por ejemplo,
--   mayorSalto [1,5]               ==  4
--   mayorSalto [10,-10,1,4,20,-2]  ==  22
-- ----------------------------------------------------------------------------

{-

mayorSalto :: [Int] -> Int
mayorSalto [] = 0
mayorSalto [s] = s
mayorSalto [x,y] = y-x
mayorSalto (x:xs) = max []
-}

-- ----------------------------------------------------------------------------
-- Ejercicio 26. Definir por comprensión la función
--   longCamino :: [(Float,Float)] -> Float
-- tal que '(longCamino xs)' es la longitud del camino determinado por los
-- puntos del plano listados en 'xs'. Por ejemplo,
--   longCamino [(0,0),(1,0),(2,1),(2,0)]  ==  3.4142137
-- ----------------------------------------------------------------------------

longCamino :: [(Float,Float)] -> Float
longCamino xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 27. Definir por comprensión la función
--   numeroConsecutivos :: (Num a, Eq a) => [a] -> Int
-- tal que '(numeroConsecutivos xs)' es la cantidad de números consecutivos
-- que aparecen al comienzo de la lista xs. Por ejemplo,
--   numeroConsecutivos [1,3,5,7,9]      ==  1
--   numeroConsecutivos [1,2,3,4,5,7,9]  ==  5
--   numeroConsecutivos [1,2,3,5,6,7]    ==  3
--   numeroConsecutivos []               ==  0
-- ----------------------------------------------------------------------------

numeroConsecutivos :: (Num a, Eq a) => [a] -> Int
numeroConsecutivos xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 28. Definir por comprensión la función
--   sumaEquidistantes :: Num a => [a] -> [a]
-- tal que '(sumaEquidistantes xs)' es la lista obtenida sumando el primer
-- elemento de 'xs' con el último, el segundo con el penúltimo y así
-- sucesivamente. Por ejemplo,
--   sumaEquidistantes [6,5,3,1]              ==  [7,8]
--   sumaEquidistantes [6,5,3]                ==  [9,10]
--   sumaEquidistantes [3,2,3,2]              ==  [5,5]
--   sumaEquidistantes [6,5,3,1,2,0,4,7,8,9]  ==  [15,13,10,5,2]
-- ----------------------------------------------------------------------------

sumaEquidistantes :: Num a => [a] -> [a]
sumaEquidistantes [] = []
sumaEquidistantes [x] = [x]
sumaEquidistantes (x:xs) = x+last xs : sumaEquidistantes (init xs)

-- ----------------------------------------------------------------------------
-- Ejercicio 29. [De la IMO 1996] Una lista [a(0),a(1),...,a(n)] se denomina
-- cuadrática si para cada i ∈ {1, 2,..., n} se cumple que
--   |a(i) − a(i−1)| = i²
--
-- Definir por comprensión la función
--   esCuadratica :: [Int] -> Bool
-- tal que '(esCuadratica xs)' se verifica si la lista 'xs' cuadrática. Por
-- ejemplo,
--   esCuadratica [2,1,-3,6]                       ==  True
--   esCuadratica [2,1,3,5]                        ==  False
--   esCuadratica [3,4,8,17,33,58,94,45,-19,-100]  ==  True
-- ----------------------------------------------------------------------------

esCuadratica :: [Int] -> Bool
esCuadratica xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 30. Dos números positivos están encadenados si la última cifra del
-- primero coincide con la primera cifra del segundo.
--
-- Definir por comprensión la función
--   encadenado :: [Int] -> Bool
-- tal que '(encadenado xs)' se verifica si 'xs' es una lista de números
-- positivos encadenados. Por ejemplo,
--   encadenado [711,1024,413,367]  ==  True
--   encadenado [711,1024,213,367]  ==  False
-- ----------------------------------------------------------------------------

encadenado :: [Int] -> Bool
encadenado [] = False
encadenado [x] = True
encadenado (x:xs) = last(show x) == head(show(head xs)) && encadenado xs

-- ============================================================================

