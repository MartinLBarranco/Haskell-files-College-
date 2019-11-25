-- I1M: Relacion_09.hs
-- Definiciones por recursión con acumulador y plegado
-- Departamento de Ciencias de la Computación e Inteligencia Artificial
-- Universidad de Sevilla
-- ============================================================================

-- ============================================================================
-- Librerías auxiliares
-- ============================================================================

import Test.QuickCheck

-- ----------------------------------------------------------------------------
-- Ejercicio 1.1. Se considera la función
--   inversa :: [a] -> [a]
-- tal que '(inversa xs)' es la inversa de la lista 'xs'. Por ejemplo,
--   inversa [3,5,2,4,7]  ==  [7,4,2,5,3]
--
-- Definir esta función
-- 1) por recursión,
-- 2) por plegado (con 'foldr'),
-- 3) por recursión con acumulador y
-- 4) por plegado (con 'foldl').
-- ----------------------------------------------------------------------------

-- 1) La definición por recursión es
inversaR :: [a] -> [a]
inversaR xs = undefined

-- 2) La definición por plegado (con 'foldr') es
inversaP :: [a] -> [a]
inversaP xs = undefined

-- 3) La definición por recursión con acumulador es
inversaAR :: [a] -> [a]
inversaAR xs = undefined

-- 4) La definición por plegado (con 'foldl') es
inversaAP :: [a] -> [a]
inversaAP xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 1.2. Comprobar con QuickCheck que la inversa de la inversa de una
-- lista coincide con la original.
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_inversa :: Eq a => [a] -> Bool
prop_inversa xs = undefined

-- La comprobación es
--   > quickCheck prop_inversa

-- ----------------------------------------------------------------------------
-- Ejercicio 1.3. Comparar la eficiencia de las funciones 'inversaP' e
-- 'inversaAP' calculando el tiempo y el espacio usado en evaluar las
-- siguientes expresiones:
--   sum (inversaP [1..10000])
--   sum (inversaAP [1..10000])
-- Nota: Para obtener información sobre el tiempo y el espacio usado en la
-- evaluación de una expresión hay que evaluar ':set +s'.
-- ----------------------------------------------------------------------------

-- La comprobación es
--   > sum (inversaP [1..10000])

--   > sum (inversaAP [1..10000])

-- ----------------------------------------------------------------------------
-- Ejercicio 2. Se considera la función
--   resultadoPositivo :: (a -> Integer) -> [a] -> [a]
-- tal que '(resultadoPositivo f xs)' es la lista de los elementos de la lista
-- 'xs' tales que el valor de la función 'f' sobre ellos es positivo. Por
-- ejemplo,
--   resultadoPositivo head [[-1,2],[-9,4],[2,3]]       ==  [[2,3]]
--   resultadoPositivo sum [[1,2],[9],[-8,3],[],[3,5]]  ==  [[1,2],[9],[3,5]]
--
-- Definir esta función
-- 1) por recursión,
-- 2) por plegado (con 'foldr'),
-- 3) por recursión con acumulador y
-- 4) por plegado (con 'foldl').
-- ----------------------------------------------------------------------------

-- 1) La definición por recursión es
resultadoPositivoR :: (a -> Integer) -> [a] -> [a]
resultadoPositivoR f xs = undefined

-- 2) La definición por plegado (con 'foldr') es
resultadoPositivoP :: (a -> Integer) -> [a] -> [a]
resultadoPositivoP f xs = undefined

-- 3) La definición por recursión con acumulador es
resultadoPositivoAR :: (a -> Integer) -> [a] -> [a]
resultadoPositivoAR f xs = undefined

-- 4) La definición por plegado (con 'foldl') es
resultadoPositivoAP :: (a -> Integer) -> [a] -> [a]
resultadoPositivoAP f xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 3. Se considera la función
--   trenza :: [a] -> [a] -> [a]
-- tal que '(trenza xs ys)' es la lista obtenida intercalando los elementos de
-- las listas 'xs' e 'ys' hasta la longitud de la más corta. Por ejemplo,
--   trenza [5,1] [2,7,4]  ==  [5,2,1,7]
--   trenza [5,1,7] [2..]  ==  [5,2,1,3,7,4]
--   trenza [2..] [5,1,7]  ==  [2,5,3,1,4,7]
--
-- Definir esta función
-- 1) por recursión,
-- 2) por plegado (con 'foldr'),
-- 3) por recursión con acumulador y
-- 4) por plegado (con 'foldl').
-- ----------------------------------------------------------------------------

-- 1) La definición por recursión es
trenzaR :: [a] -> [a] -> [a]
trenzaR [] ys = []
trenzaR xs [] = []
trenzaR (x:xs) ys = x:(trenzaR ys xs)

-- 2) La definición por plegado (con 'foldr') es
trenzaP :: [a] -> [a] -> [a]
trenzaP xs ys = foldr (\(x,y) xs -> [x,y] ++ xs) [] (zip xs ys)

-- 3) La definición por recursión con acumulador es
trenzaAR :: [a] -> [a] -> [a]
trenzaAR xs ys = trenAux xs ys []


trenAux [] ys  ac = []
trenAux (x:xs) (y:ys) ac = trenAux xs ys (ac++[x,y])

-- 4) La definición por plegado (con 'foldl') es
trenzaAP :: [a] -> [a] -> [a]
trenzaAP xs ys = foldl (\ xs (x,y) -> xs++[x,y]) [] (zip xs ys)

-- ----------------------------------------------------------------------------
-- Ejercicio 4. Se considera la función
--   intercala :: Int -> [Int] -> [Int]
-- tal que '(intercala y xs)' es la lista que resulta de intercalar el elemento
-- 'y' delante de todos los elementos de la lista 'xs' que sean menores que
-- 'y'. Por ejemplo,
--   intercala 5 [1,2,6,3,7,9]  ==  [5,1,5,2,6,5,3,7,9]
--   intercala 5 [6,7,9,8]      ==  [6,7,9,8]
--
-- Definir esta función
-- 1) por recursión,
-- 2) por plegado (con 'foldr'),
-- 3) por recursión con acumulador y
-- 4) por plegado (con 'foldl').
-- ----------------------------------------------------------------------------

-- 1) La definición por recursión es
intercalaR :: Int -> [Int] -> [Int]
intercalaR y [] = []
intercalaR y (x:xs) |y<x = [y,x]++intercalaR y xs
                    |otherwise = x:intercalaR y xs

-- 2) La definición por plegado (con 'foldr') es
intercalaP :: Int -> [Int] -> [Int]
intercalaP y xs = foldr (\x xs -> if y>x then [y,x]++xs else x:xs) [] xs

-- 3) La definición por recursión con acumulador es
intercalaAR :: Int -> [Int] -> [Int]
intercalaAR y xs = interAux y xs []

interAux y [] ac = ac
interAux y (x:xs) ac |y>x = interAux y xs (ac++[y,x])
                     |otherwise = interAux y xs (ac++[x])


-- 4) La definición por plegado (con 'foldl') es
intercalaAP :: Int -> [Int] -> [Int]
intercalaAP y xs = foldl (\xs x -> if y>x then [y,x]++xs else xs++[x]) [] xs

-- ----------------------------------------------------------------------------
-- Ejercicio 5. Se considera la función
--   alternos :: (a -> b) -> (a -> b) -> [a] -> [b]
-- tal que '(alternos f g xs)' es la lista obtenida aplicando alternativamente
-- las funciones 'f' y 'g' a los elementos de la lista 'xs'. Por ejemplo,
--   alternos (+1) (*3) [1,2,3,4,5]                    ==  [2,6,4,12,6]
--   alternos (take 2) reverse ["todo","para","nada"]  ==  ["to","arap","na"]
--
-- Definir esta función
-- 1) por recursión,
-- 2) por plegado (con 'foldr'),
-- 3) por recursión con acumulador y
-- 4) por plegado (con 'foldl').
-- ----------------------------------------------------------------------------

-- 1) La definición por recursión es
alternosR :: (a -> b) -> (a -> b) -> [a] -> [b]
alternosR f g [] = []
alternosR f g (x:xs) = f x : alternosR g f xs

-- 2) La definición por plegado (con 'foldr') es
--alternosP :: (a -> b) -> (a -> b) -> [a] -> [b]
--alternosP f g xs = foldr (\ x xs) [] xs
 
-- 3) La definición por recursión con acumulador es
alternosAR :: (a -> b) -> (a -> b) -> [a] -> [b]
alternosAR f g xs = alterAux f g xs []

alterAux f g [] ac = ac
alterAux f g (x:xs) ac = alterAux g f xs (ac++[f x]) 


-- 4) La definición por plegado (con 'foldl') es
--alternosAP :: (a -> b) -> (a -> b) -> [a] -> [b]
--alternosAP f g xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 6. Se considera la función
--   dec2ent :: [Int] -> Int
-- tal que '(dec2ent xs)' es el número entero cuyas cifras ordenadas son los
-- elementos de la lista 'xs'. Por ejemplo,
--   dec2ent [2,3,4,5]  ==  2345
--   dec2ent [1..9]     ==  123456789
--
-- Definir esta función
-- 1) por recursión con acumulador y
-- 2) por plegado (con 'foldl').
-- ----------------------------------------------------------------------------

-- 1) La definición por recursión con acumulador es
dec2entAR :: [Int] -> Int
dec2entAR xs = aux xs 0

aux [] ac = ac
aux (x:xs) ac = aux xs (10*ac+x)

-- 2) La definición por plegado (con 'foldl') es
dec2entAP :: [Int] -> Int
dec2entAP xs = foldl (\ r x -> r*10+x) 0 xs

-- ----------------------------------------------------------------------------
-- Ejercicio 7. Se considera la función
--   sumll :: Num a => [[a]] -> a
-- tal que '(sumll xss)' es la suma de las sumas de las listas de 'xss'. Por
-- ejemplo,
--   sumll [[1,3],[2,5]]              ==  11
--   sumll [[1,3,5],[2,4,6,8],[5,3]]  ==  37
--
-- Definir esta función
-- 1) por recursión con acumulador y
-- 2) por plegado (con 'foldl').
-- ----------------------------------------------------------------------------

-- 1) La definición por recursión con acumulador es
sumllAR :: Num a => [[a]] -> a
sumllAR xss = sumAux xss 0

sumAux [] ac = ac
sumAux (xs:xss) ac = sumAux xss (ac + sum xs)

-- 2) La definición por plegado (con 'foldl') es
sumllAP :: Num a => [[a]] -> a
sumllAP xss = foldl (\ xss xs -> xss +sum xs) 0 xss

-- ----------------------------------------------------------------------------
-- Ejercicio 8. Se considera la función
--   elimina :: Eq a => a -> a -> [a]
-- tal que '(elimina y xs)' es la lista obtenida borrando todas las ocurrencias
-- de 'y' en la lista 'xs'. Por ejemplo,
--   elimina 1 [1,2,1,3,1,4]  ==  [2,3,4]
--   elimina 3 [1,2,1]        ==  [1,2,1]
--
-- Definir esta función
-- 1) por recursión con acumulador y
-- 2) por plegado (con 'foldl').
-- ----------------------------------------------------------------------------

-- 1) La definición por recursión con acumulador es
eliminaAR :: Eq a => a -> [a] -> [a]
eliminaAR y xs = eliAux y xs []

eliAux y [] ac = ac
eliAux y (x:xs) ac |y/=x = eliAux y xs (ac++[x])
                   |otherwise = eliAux y xs ac

-- 2) La definición por plegado (con 'foldl') es
eliminaAP :: Eq a => a -> [a] -> [a]
eliminaAP y xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 9. Se considera la función
--   diferencia :: Eq a => [a] -> [a] -> [a]
-- tal que '(diferencia xs ys)' es la diferencia entre los conjuntos 'xs' e
-- 'ys'; es decir el conjunto de los elementos de la lista 'xs' que no se
-- encuentran en la lista 'ys'. Por ejemplo,
--   diferencia [2,3,5,6] [5,2,7]  ==  [3,6]
--   diferencia [1,3,5,7] [2,4,6]  ==  [1,3,5,7]
--   diferencia [1,3] [1..9]       ==  []
--
-- Definir esta función
-- 1) por recursión con acumulador y
-- 2) por plegado (con 'foldl').
-- ----------------------------------------------------------------------------

-- 1) La definición por recursión con acumulador es
diferenciaAR :: Eq a => [a] -> [a] -> [a]
diferenciaAR xs ys = undefined

-- 2) La definición por plegado (con 'foldl') es
diferenciaAP :: Eq a => [a] -> [a] -> [a]
diferenciaAP xs ys = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 10. Un elemento de una lista es permanente si ninguno de los que
-- vienen a continuación en la lista es mayor que él.
--
-- Consideramos la función
--   permanentes :: [Int] -> [Int]
-- tal que '(permanentes xs)' es la lista de los elementos permanentes de la
-- lista 'xs'. Por ejemplo,
--   permanentes [80,1,7,8,4]  ==  [80,8,4]
--
-- Definir esta función
-- 1) por recursión con acumulador y
-- 2) por plegado (con 'foldl').
-- ----------------------------------------------------------------------------

-- 1) La definición por recursión con acumulador es
permanentesAR :: [Int] -> [Int]
permanentesAR xs = undefined

-- 2) La definición por plegado (con 'foldl') es
permanentesAP :: [Int] -> [Int]
permanentesAP xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 11. Se denomina cola de una lista a una sublista no vacía formada
-- por un elemento y los siguientes hasta el final. Por ejemplo, [3,4,5] es una
-- cola de la lista [1,2,3,4,5].
--
-- Consideramos la función
--   colas :: [a] -> [[a]]
-- tal que '(colas xs)' es la lista de las colas de la lista 'xs'. Por ejemplo,
--   colas []         ==  []
--   colas [1,2]      ==  [[1,2],[2]]
--   colas [4,1,2,5]  ==  [[4,1,2,5],[1,2,5],[2,5],[5]]
--
-- Definir esta función
-- 1) por recursión con acumulador y
-- 2) por plegado (con 'foldl').
-- ----------------------------------------------------------------------------

-- 1) La definición por recursión con acumulador es
colasAR :: [a] -> [[a]]
colasAR xs = undefined

-- 2) La definición por plegado (con 'foldl') es
colasAP :: [a] -> [[a]]
colasAP xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 12. Se considera la función
--   primerosYultimos :: [[a]] -> ([a],[a])
-- tal que '(primerosYultimos xss)' es el par formado por la lista de los
-- primeros elementos de las listas no vacías de 'xss' y la lista de los
-- últimos elementos de las listas no vacías de 'xss'. Por ejemplo,
--   primerosYultimos [[1,2],[5,3,4],[],[9]]  ==  ([1,5,9],[2,4,9])
--   primerosYultimos [[1,2],[1,2,3],[1..4]]  ==  ([1,1,1],[2,3,4])
--
-- Definir esta función
-- 1) por recursión con acumulador y
-- 2) por plegado (con 'foldl').
-- ----------------------------------------------------------------------------

-- 1) La definición por recursión con acumulador es
primerosYultimosAR :: [[a]] -> ([a],[a])
primerosYultimosAR xss = undefined

-- 2) La definición por plegado (con 'foldl') es
primerosYultimosAP :: [[a]] -> ([a],[a])
primerosYultimosAP xss = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 13. La sucesión de Fibonacci es
--   1, 1, 2, 3, 5, 8, 13, ...
-- es decir, los dos primeros valores son 1 y a partir de ahí cualquier término
-- de la sucesión es igual a la suma de los dos anteriores.
--
-- Consideramos la función
--   fibonacci :: Integer -> Integer
-- tal que '(fibonacci n)' es el 'n'-ésimo término de la sucesión de Fibonacci.
-- Por ejemplo,
--   fibonacci 1   ==  1
--   fibonacci 5   ==  5
--   fibonacci 10  ==  55
--
-- Definir esta función
-- 1) por recursión con acumulador y
-- 2) por plegado (con 'foldl').
-- ----------------------------------------------------------------------------

-- 1) La definición por recursión con acumulador es
fibonacciAR :: Integer -> Integer
fibonacciAR n = undefined

-- 2) La definición por plegado (con 'foldl') es
fibonacciAP :: Integer -> Integer
fibonacciAP n = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 14. Un montón de barriles se construye apilando unos encima de
-- otros por capas, de forma que en cada capa todos los barriles están apoyados
-- sobre dos de la capa inferior y todos los barriles de una misma capa están
-- pegados unos a otros. Por ejemplo, los siguientes montones son válidos:
--             _          _   _                   _
--            / \        / \ / \                 / \
--           _\_/_      _\_/_\_/_   _       _   _\_/_   _
--          / \ / \    / \ / \ / \ / \     / \ / \ / \ / \
--          \_/ \_/    \_/ \_/ \_/ \_/     \_/ \_/ \_/ \_/
--
-- y los siguientes no son válidos:
--         _   _          _       _               _   _
--        / \ / \        / \     / \             / \ / \
--        \_/_\_/_      _\_/_   _\_/_       _   _\_/_\_/
--          / \ / \    / \ / \ / \ / \     / \ / \ / \
--          \_/ \_/    \_/ \_/ \_/ \_/     \_/ \_/ \_/
--
-- Se puede comprobar que el número de formas distintas de construir montones
-- con n barriles en la base (M_n) viene dado por la siguiente fórmula:
--
--              (n-1)
--             -------
--              \
--               \
--   M_n = 1 +    )    (n-j) * M_j
--               /
--              /
--             -------
--              j = 1
--
-- Consideremos la función
--   montones :: Integer -> Integer
-- tal que '(montones n)' es el número de formas distintas de construir
-- montones con 'n' barriles en la base. Por ejemplo,
--   montones 1   ==  1
--   montones 10  ==  4181
--   montones 20  ==  63245986
--   montones 30  ==  956722026041
--
-- Definir esta función
-- 1) por recursión con acumulador y
-- 2) por plegado (con 'foldl').
--
-- Calcular el número de formas distintas de construir montones con 50 barriles
-- en la base.
-- ----------------------------------------------------------------------------

-- 1) La definición por recursión con acumulador es
montonesAR :: Integer -> Integer
montonesAR n = undefined

-- 2) La definición por plegado (con 'foldl') es
montonesAP :: Integer -> Integer
montonesAP n = undefined

-- El cálculo es
--   > montonesAP 50

-- ----------------------------------------------------------------------------
-- Ejercicio 15. La sucesión generalizada de Fibonacci de grado N (N >= 1) se
-- construye comenzando con el número 1 y calculando el resto de términos como
-- la suma de los N términos anteriores (si existen). Por ejemplo,
-- * la sucesión generalizada de Fibonacci de grado 2 es:
--   1, 1, 2, 3, 5, 8, 13, 21, 34, 55
-- * la sucesión generalizada de Fibonacci de grado 4 es:
--   1, 1, 2, 4, 8, 15, 29, 56, 108, 208
-- * la sucesión generalizada de Fibonacci de grado 6 es:
--   1, 1, 2, 4, 8, 16, 32, 63, 125, 248
--
-- Consideramos la función
--   fibonacciGeneralizada :: Int -> Integer -> Integer
-- tal que '(fibonacciGeneralizada n m)' es el término 'm' de la sucesión
-- generalizada de Fibonacci de grado 'n'. Por ejemplo,
--   fibonacciGeneralizada 4 3  ==  4
--   fibonacciGeneralizada 4 4  ==  8
--   fibonacciGeneralizada 4 5  ==  15
--   fibonacciGeneralizada 5 7  ==  61
--   fibonacciGeneralizada 5 8  ==  120
--   fibonacciGeneralizada 5 9  ==  236
--
-- Definir esta función
-- 1) por recursión con acumulador y
-- 2) por plegado (con 'foldl').
-- ----------------------------------------------------------------------------

-- 1) La definición por recursión con acumulador es
fibonacciGeneralizadaAR :: Int -> Integer -> Integer
fibonacciGeneralizadaAR n m = undefined

-- 2) La definición por plegado (con 'foldl') es
fibonacciGeneralizadaAP :: Int -> Integer -> Integer
fibonacciGeneralizadaAP n m = undefined

-- ============================================================================
