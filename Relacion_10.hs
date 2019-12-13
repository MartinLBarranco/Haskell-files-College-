-- I1M: Relacion-10.hs
-- Listas infinitas y evaluación perezosa
-- Departamento de Ciencias de la Computación e Inteligencia Artificial
-- Universidad de Sevilla
-- ============================================================================

--Esta prohibido usar sum, product, length, last, maximum/minimum, last,
--reverse, init

--Es peligroso usar (ya que el resultado es tambien una lista infinita): tail,
--drop, dropWhile, map, foldr/l, filter, any, all, or, zip, elem

--Es seguro de usar take, !!, takeWhile


--Tipos de ejercicios:
--Por compresion
--Por recursion



-- ============================================================================
-- Librerías auxiliares
-- ============================================================================

import Test.QuickCheck
import Data.Numbers.Primes

-- ----------------------------------------------------------------------------
-- Ejercicio 1. Consideramos la función
--   repite :: a -> [a]
-- tal que '(repite x)' es la lista infinita cuyos elementos son todos iguales
-- a 'x'. Por ejemplo,
--   repite 5           ==  [5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,...
--   take 3 (repite 5)  ==  [5,5,5]
--
-- Definir esta función
-- 1) por comprensión sobre una lista infinita y
-- 2) por recursión infinita.
--
-- Nota: La función 'repite' es equivalente a la función 'repeat' definida
-- en el preludio de Haskell.
-- ----------------------------------------------------------------------------

-- 1) La definición por comprensión sobre una lista infinita es
repiteC :: a -> [a]
repiteC x = [x | i<-[1..]]

-- 2) La definición por recursión infinita es
repiteR :: a -> [a]
repiteR x = x :repiteR x

-- ----------------------------------------------------------------------------
-- Ejercicio 2.1. Consideramos la función
--   repiteFinita :: Int -> a -> [a]
-- tal que '(repiteFinita n x)' es la lista con 'n' elementos iguales a 'x'.
-- Por ejemplo,
--   repiteFinita 3 5  ==  [5,5,5]
--
-- Definir esta función
-- 1) por comprensión,
-- 2) por recursión y
-- 3) usando la función 'repeat'.
--
-- Nota: La función 'repiteFinita' es equivalente a la función 'replicate'
-- definida en el preludio de Haskell.
-- ----------------------------------------------------------------------------

-- 1) La definición por comprensión es
repiteFinitaC :: Int -> a -> [a]
repiteFinitaC n x = [x | i<-[1..n]]

-- 2) La definición por recursión es
repiteFinitaR :: Int -> a -> [a]
repiteFinitaR 1 x = [x]
repiteFinitaR n x = x:repiteFinitaR (n-1) x

-- 3) La definición usando la función 'repeat' es
repiteFinita :: Int -> a -> [a]
repiteFinita n x = take n (repeat x)

-- ----------------------------------------------------------------------------
-- Ejercicio 2.2. Comprobar con QuickCheck que las funciones 'repiteFinitaR',
-- 'repiteFinitaC' y 'repiteFinita' son equivalentes a 'replicate'.
--
-- Nota. Al hacer la comprobación limitar el tamaño de las pruebas como se
-- indica a continuación
--   quickCheckWith (stdArgs {maxSize=7}) prop_repiteFinitaEquiv
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_repiteFinitaEquiv :: Int -> Int -> Bool
prop_repiteFinitaEquiv n x = undefined

-- La comprobación es
--   > quickCheckWith (stdArgs {maxSize=20}) prop_repiteFinitaEquiv

-- ----------------------------------------------------------------------------
-- Ejercicio 2.3. Comprobar con QuickCheck que la longitud del resultado de
-- '(repiteFinita n x)' es 'n', si 'n' es positivo y 0 si no lo es.
--
-- Nota. Al hacer la comprobación limitar el tamaño de las pruebas como se
-- indica a continuación
--   quickCheckWith (stdArgs {maxSize=30}) prop_repiteFinitaLongitud
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_repiteFinitaLongitud :: Int -> Int -> Bool
prop_repiteFinitaLongitud n x = undefined

-- La comprobación es
--   > quickCheckWith (stdArgs {maxSize=30}) prop_repiteFinitaLongitud

-- ----------------------------------------------------------------------------
-- Ejercicio 2.4. Comprobar con QuickCheck que todos los elementos del
-- resultado de '(repiteFinita n x)' son iguales a 'x'.
--
-- Nota. Al hacer la comprobación limitar el tamaño de las pruebas como se
-- indica a continuación
--   quickCheckWith (stdArgs {maxSize=30}) prop_repiteFinitaIguales
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_repiteFinitaIguales :: Int -> Int -> Bool
prop_repiteFinitaIguales n x = undefined

-- La comprobación es
--   > quickCheckWith (stdArgs {maxSize=30}) prop_repiteFinitaIguales

-- ----------------------------------------------------------------------------
-- Ejercicio 3. Consideramos la función
--   eco :: String -> String
-- tal que '(eco xs)' es la cadena obtenida a partir de la cadena 'xs'
-- repitiendo cada elemento tantas veces como indica su posición: el primer
-- elemento se repite 1 vez, el segundo 2 veces y así sucesivamente. Por
-- ejemplo,
--   eco "abcd"  ==  "abbcccdddd"
--
-- Definir esta función
-- 1) por comprensión y
-- 2) por recursión.
-- ----------------------------------------------------------------------------

-- 1) La definición por comprensión es
ecoC :: String -> String
ecoC xs = concat[repiteFinitaC i n | (i,n)<- zip [1..(length xs)] xs]

-- 2) La definición por recursión es
ecoR :: String -> String
ecoR = reverse . ecoRAux . reverse

ecoRAux :: String -> String
ecoRAux "" = ""
ecoRAux (x:xs) = repiteFinita (length xs +1) x ++ ecoRAux xs

-- ----------------------------------------------------------------------------
-- Ejercicio 4. Definir por recursión infinita la función
--   itera :: (a -> a) -> a -> [a]
-- tal que '(itera f x)' es la lista cuyo primer elemento es 'x' y los
-- siguientes elementos se calculan aplicando la función 'f' al elemento
-- anterior. Por ejemplo,
--   take 10 (itera (+1) 3)           ==  [3,4,5,6,7,8,9,10,11,12]
--   take 10 (itera (*2) 1)           ==  [1,2,4,8,16,32,64,128,256,512]
--   take 10 (itera (`div` 10) 1972)  ==  [1972,197,19,1,0,0,0,0,0,0]
--
-- Nota: La función 'itera' es equivalente a la función 'iterate' definida en
-- el preludio de Haskell.
-- ----------------------------------------------------------------------------

itera :: (a -> a) -> a -> [a]
itera f x = x : itera f (f x)

-- ----------------------------------------------------------------------------
-- Ejercicio 5. Consideramos la función
--   potenciaFuncional :: Int -> (a -> a) -> a -> a
-- tal que '(potenciaFuncional n f x)' es el resultado de aplicar 'n' veces la
-- función 'f' a 'x'. Por ejemplo,
--   potenciaFuncional 3 (*10) 5  ==  5000
--   potenciaFuncional 4 (+10) 5  ==  45
--
-- Definir esta función
-- 1) por recursión y
-- 2) usando la función 'iterate'.
-- ----------------------------------------------------------------------------

-- 1) La definición por recursión es
potenciaFuncionalR :: Int -> (a -> a) -> a -> a
potenciaFuncionalR n f x = last (take (n+1) (itera f x))

-- 2) La definición usando la función 'iterate' es
potenciaFuncional :: Int -> (a -> a) -> a -> a
potenciaFuncional n f x = last (take (n+1) (iterate f x))

-- ----------------------------------------------------------------------------
-- Ejercicio 6. Los números enteros se pueden ordenar de la siguiente forma
--   0, -1, 1, -2, 2, -3, 3, -4, 4, -5, 5, -6, 6, -7, 7, ...
--
-- Consideramos la constante
--   enterosOrdenados :: [Int]
-- cuyo valor es la lista de los enteros con la ordenación anterior. Por
-- ejemplo,
--   take 10 enterosOrdenados  ==  [0,-1,1,-2,2,-3,3,-4,4,-5]
--
-- Definir esta constante
-- 1) por comprensión sobre una lista infinita y
-- 2) usando la función 'iterate'.
-- ----------------------------------------------------------------------------

-- 1) La definición por comprensión sobre una lista infinita es
enterosOrdenadosC :: [Int]
enterosOrdenadosC = undefined

-- 2) La definición usando la función 'iterate' es
enterosOrdenados :: [Int]
enterosOrdenados = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 7. Consideramos la función
--   posicionT :: Int -> Int
-- tal que '(posicionT x)' es la posición del entero 'x' en la ordenación del
-- ejercicio anterior. Por ejemplo,
--   posicion 0     ==  0
--   posicion (-2)  ==  3
--   posicion 2     ==  4
--
-- Definir esta función
-- 1) por comprensión sobre una lista infinita,
-- 2) por recursión,
-- 3) usando la función 'takeWhile' y
-- 4) sin usar la constante definida en el ejercicio anterior.
-- ----------------------------------------------------------------------------

yea :: Int -> Int
yea n | n==0 = 0
      | n>0 = 2*n
      | otherwise = yea (-n) -1


-- 1) La definición por comprensión sobre una lista infinita es
posicionC :: Int -> Int
posicionC x = undefined

-- 2) La definición por recursión es
posicionR :: Int -> Int
posicionR x = undefined

-- 3) La definición usando la función 'takeWhile' es
posicionT :: Int -> Int
posicionT x = undefined

-- 4) La definición sin usar la constante definida en el ejercicio anterior es
posicion :: Int -> Int
posicion x = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 8. Definir usando listas infinitas la función
--   potenciasMenores :: Int -> Int -> [Int]
-- tal que '(potenciasMenores x y)' es la lista de las potencias de 'x' menores
-- que 'y'. Por ejemplo,
--   potenciasMenores 2 1000  ==  [2,4,8,16,32,64,128,256,512]
-- ----------------------------------------------------------------------------

potenciasMenores :: Int -> Int -> [Int]
potenciasMenores x y = takeWhile (<=y) [x^i | i<- [1..]]

-- ----------------------------------------------------------------------------
-- Ejercicio 9. Definir usando listas infinitas la función
--   perteneceRango :: Int -> (Int -> Int) -> Bool
-- tal que '(perteneceRango x f)' se verifica si 'x' pertenece al rango de la
-- función 'f', suponiendo que 'f' es una función creciente cuyo dominio es el
-- conjunto de los números naturales. Por ejemplo,
--   perteneceRango 5 (\x -> 2*x+1)     ==  True
--   perteneceRango 1234 (\x -> 2*x+1)  ==  False
-- ----------------------------------------------------------------------------

perteneceRango :: Int -> (Int -> Int) -> Bool
perteneceRango x f = elem x xs
           where xs = takeWhile (>x) (map f [1..])      --ESTA MAL

-- ----------------------------------------------------------------------------
-- Ejercicio 10.1. Consideramos la constante
--   factoriales :: [Integer]
-- cuyo valor es la lista infinita de los factoriales. Por ejemplo,
--   take 10 factoriales  ==  [1,1,2,6,24,120,720,5040,40320,362880]
--
-- Definir esta constante
-- 1) por comprensión sobre una lista infinita,
-- 2) por recursión infinita y
-- 3) usando la función 'iterate'.
-- ----------------------------------------------------------------------------

-- 1) La definición por comprensión sobre una lista infinita es
factorialesC :: [Integer]
factorialesC = undefined

-- 2) La definición por recursión infinita es
factorialesR :: [Integer]
factorialesR = undefined

-- 3) La definición usando la función 'iterate' es
factorialesI :: [Integer]
factorialesI = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 10.2. Comparar el tiempo y espacio necesarios para calcular la
-- expresión
--   let xs = take 3000 factoriales in (sum xs - sum xs)
-- con cada una de las dos definiciones anteriores
-- ----------------------------------------------------------------------------

-- El cálculo es
--   > :set +s
--   > let xs = take 3000 factorialesC in (sum xs - sum xs)
--   > let xs = take 3000 factorialesR in (sum xs - sum xs)
--   > let xs = take 3000 factorialesI in (sum xs - sum xs)

-- ----------------------------------------------------------------------------
-- Ejercicio 11. La sucesión de Fibonacci está definida por
--   f(0) = 0
--   f(1) = 1
--   f(n) = f(n-1)+f(n-2), si n > 1.
--
-- Definir la función 'fibonacci', tal que '(fibonacci n)' es el n-ésimo
-- término de la sucesión de Fibonacci. Por ejemplo,
--   fibonacci 8  ==  21
-- ----------------------------------------------------------------------------

fibonacci :: Integer -> Integer
fibonacci n = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 12.1. Consideramos la constante
--   sucesionFibonacci :: [Integer]
-- cuyo valor es la sucesión infinita de Fibonacci. Por ejemplo,
--   take 10 sucesionFibonacci  ==  [0,1,1,2,3,5,8,13,21,34]
--
-- Definir esta constante
-- 1) por comprensión sobre una lista infinita,
-- 2) por recursión con acumuladores y
-- 3) por recursión infinita.
-- ----------------------------------------------------------------------------

-- 1) La definición por comprensión sobre una lista infinita es
sucesionFibonacciC :: [Integer]
sucesionFibonacciC = undefined

-- 2) La definición por recursión con acumuladores es
sucesionFibonacciRA :: [Integer]
sucesionFibonacciRA = undefined

-- 3) La definición por recursión infinita es
sucesionFibonacciR :: [Integer]
sucesionFibonacciR = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 12.2. Comparar el tiempo y espacio necesarios para calcular la
-- expresión
--   map (sucesionFibonacci !!) [5,10,15,20,25,30,35]
-- con cada una de las definiciones anteriores
-- ----------------------------------------------------------------------------

-- El cálculo es
--   > :set +s
--   > map (sucesionFibonacciC !!) [5,10,15,20,25,30,35]
--   > map (sucesionFibonacciRA !!) [5,10,15,20,25,30,35]
--   > map (sucesionFibonacciR !!) [5,10,15,20,25,30,35]

-- ----------------------------------------------------------------------------
-- Ejercicio 13. Cuentan que Alan Turing tenía una bicicleta vieja, que tenía
-- una cadena con un eslabón débil y además uno de los radios de la rueda
-- estaba doblado. Cuando el radio doblado coincidía con el eslabón débil,
-- entonces la cadena se rompía.
--
-- La bicicleta se identifica por los parámetros (i,d,n) donde:
-- * i es el número del eslabón que coincide con el radio doblado al empezar a
--   andar,
-- * d es el número de eslabones que se desplaza la cadena en cada vuelta de la
--   rueda y
-- * n es el número de eslabones de la cadena (el número n es el débil).
-- Si i=2 y d=7 y n=25, entonces la lista con el número de eslabón que toca el
-- radio doblado en cada vuelta es
--   [2,9,16,23,5,12,19,1,8,15,22,4,11,18,0,7,14,21,3,10,17,24,6,...
-- Con lo que la cadena se rompe en la vuelta número 14.
--
-- Definir la función
--   eslabones :: Int -> Int -> Int -> [Int]
-- tal que '(eslabones i d n)' es la lista infinita con los números de
-- eslabones que tocan el radio doblado en cada vuelta en una bicicleta de tipo
-- '(i,d,n)'. Por ejemplo,
--   take 10 (eslabones 2 7 25)  ==  [2,9,16,23,5,12,19,1,8,15]
-- ----------------------------------------------------------------------------

eslabones :: Int -> Int -> Int -> [Int]
eslabones i d n = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 14. Definir la función
--   numeroVueltas :: Int -> Int -> Int -> Int
-- tal que '(numeroVueltas i d n)' es el número de vueltas que pasarán hasta
-- que la cadena se rompa en una bicicleta de tipo '(i,d,n)'. Por ejemplo,
--   numeroVueltas 2 7 25  ==  14
-- ----------------------------------------------------------------------------

numeroVueltas :: Int -> Int -> Int -> Int
numeroVueltas i d n = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 15. El triángulo de Pascal es un triángulo de números
--
--               1
--             1   1
--           1   2   1
--         1   3   3   1
--       1   4   6   4   1
--     1   5  10  10   5   1
--   .........................
--
-- construido de la siguiente forma
-- * la primera fila está formada por el número 1;
-- * las filas siguientes se construyen sumando los números adyacentes de la
-- fila superior y añadiendo un 1 al principio y al final de la fila.
--
-- Definir por recursión infinita la constante
--   trianguloPascal :: [[Integer]]
-- cuyo valor es la lista infinita de las líneas del triángulo de Pascal. Por
-- ejemplo,
--   take 6 trianguloPascal  ==
--     [[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1],[1,5,10,10,5,1]]
-- ----------------------------------------------------------------------------

trianguloPascal :: [[Integer]]
trianguloPascal = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 16. Consideremos la siguiente operación, aplicable a cualquier
-- número entero positivo:
--   * Si el número es par, se divide entre 2.
--   * Si el número es impar, se multiplica por 3 y se suma 1.
-- Dado un número cualquiera, podemos considerar su órbita, es decir, las
-- imágenes sucesivas al iterar la operación anterior. Por ejemplo, la órbita
-- de 13 es
--   13, 40, 20, 10, 5, 16, 8, 4, 2, 1, 4, 2, 1,...
-- Si observamos este ejemplo, la órbita de 13 es periódica, es decir, se
-- repite indefinidamente a partir de un momento dado). La conjetura de Collatz
-- dice que siempre alcanzaremos el 1 para cualquier número con el que
-- comencemos. Ejemplos:
--   * Empezando en n = 6 se obtiene 6, 3, 10, 5, 16, 8, 4, 2, 1.
--   * Empezando en n = 11 se obtiene: 11, 34, 17, 52, 26, 13, 40, 20, 10, 5,
--     16, 8, 4, 2, 1.
--   * Empezando en n = 27, la sucesión tiene 112 pasos, llegando hasta 9232
--     antes de descender a 1:  27, 82, 41, 124, 62, 31, 94, 47, 142, 71, 214,
--     107, 322, 161, 484, 242, 121, 364, 182, 91, 274, 137, 412, 206, 103,
--     310, 155, 466, 233, 700, 350, 175, 526, 263, 790, 395, 1186, 593, 1780,
--     890, 445, 1336, 668, 334, 167, 502, 251, 754, 377, 1132, 566, 283, 850,
--     425, 1276, 638, 319, 958, 479, 1438, 719, 2158, 1079, 3238, 1619, 4858,
--     2429, 7288, 3644, 1822, 911, 2734, 1367, 4102, 2051, 6154, 3077, 9232,
--     4616, 2308, 1154, 577, 1732, 866, 433, 1300, 650, 325, 976, 488, 244,
--     122, 61, 184, 92, 46, 23, 70, 35, 106, 53, 160, 80, 40, 20, 10, 5, 16,
--     8, 4, 2, 1.
--
-- Definir la función
--   siguienteCollatz :: Integer -> Integer
-- tal que '(siguienteCollatz n)' es el siguiente de 'n' en la sucesión de
-- Collatz. Por ejemplo,
--   siguienteCollatz 13  ==  40
--   siguienteCollatz 40  ==  20
-- ----------------------------------------------------------------------------

siguienteCollatz :: Integer -> Integer
siguienteCollatz n = if even n then n`div`2 else 3*n+1

-- ----------------------------------------------------------------------------
-- Ejercicio 17. Consideramos la función
--   orbitaCollatz :: Integer -> [Integer]
-- tal que '(orbitaCollatz n)' es la órbita de Collatz de 'n' hasta alcanzar el
-- 1. Por ejemplo,
--   orbitaCollatz 13  ==  [13,40,20,10,5,16,8,4,2,1]
--
-- Definir esta función
-- 1) por recursión y
-- 2) usando la función 'iterate'.
-- ----------------------------------------------------------------------------

-- 1) La definición por recursión es
orbitaCollatzR :: Integer -> [Integer]
orbitaCollatzR 1 = [1]
orbitaCollatzR n = n : orbitaCollatzR (siguienteCollatz n)

-- 2) La definición usando la función 'iterate' es
orbitaCollatzI :: Integer -> [Integer]
orbitaCollatzI n = takeWhile (/= 1) (iterate (siguienteCollatz) n) ++ [1]

-- ----------------------------------------------------------------------------
-- Ejercicio 18. Definir usando listas infinitas la función
--   menorCollatzMayor :: Int -> Integer
-- tal que '(menorCollatzMayor x)' es el menor número cuya órbita de Collatz
-- tiene más de 'x' elementos. Por ejemplo,
--   menorCollatzMayor 100  ==  27
-- ----------------------------------------------------------------------------
longCol :: Integer -> Integer
longCol 1 = 1
longCol n = 1+longCol(siguienteCollatz n)


menorCollatzMayor :: Int -> Integer
menorCollatzMayor x = head[n | n<-[1..], length (orbitaCollatzI n) > x ]

-- ----------------------------------------------------------------------------
-- Ejercicio 19. Definir usando listas infinitas la función
--   menorCollatzSupera :: Integer -> Integer
-- tal que '(menorCollatzSupera x)' es el menor número cuya órbita de Collatz
-- tiene algún elemento mayor que 'x'. Por ejemplo,
--   menorCollatzSupera 100  ==  15
-- ----------------------------------------------------------------------------

menorCollatzSupera :: Integer -> Integer
menorCollatzSupera x = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 20. Definir usando la criba de Eratóstenes la constante
--   primos :: [Int]
-- cuyo valor es la lista infinita de los números primos. Por ejemplo,
--   take 10 primos  ==  [2,3,5,7,11,13,17,19,23,29]
--
-- Nota: La función 'primos' es equivalente a la función 'primes' definida
-- en la librería Data.Numbers.Primes
-- ----------------------------------------------------------------------------

primos :: [Int]
primos = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 21. Definir usando la constante anterior la función
--   primo :: Int -> Bool
-- tal que '(primo n)' se verifica si 'n' es un número primo. Por ejemplo,
--   primo 7  ==  True
--   primo 9  ==  False
--
-- Nota: La función 'primo' es equivalente a la función 'isPrime' definida en
-- la librería Data.Numbers.Primes
-- ----------------------------------------------------------------------------

primo :: Int -> Bool
primo n = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 22. [Problema 10 del Proyecto Euler] Definir la función
--   sumaPrimosMenores :: Int -> Int
-- tal que '(sumaPrimosMenores n)' es la suma de los números primos menores que
-- el número 'n'. Por ejemplo,
--   sumaPrimoMenores 10  ==  17
-- ----------------------------------------------------------------------------

sumaPrimoMenores :: Int -> Int
sumaPrimoMenores n = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 23. Definir la función
--   sumaDeDosPrimos :: Int -> [(Int,Int)]
-- tal que '(sumaDeDosPrimos n)' es la lista de las distintas descomposiciones
-- de 'n' como suma de dos números primos. Por ejemplo,
--   sumaDeDosPrimos 22  ==  [(3,19),(5,17),(11,11)]
--   sumaDeDosPrimos 30  ==  [(7,23),(11,19),(13,17)]
--
-- Calcular usando esta función el menor número que puede escribirse de 10
-- formas distintas como suma de dos primos.
-- ----------------------------------------------------------------------------

sumaDeDosPrimos :: Int -> [(Int,Int)]
sumaDeDosPrimos n = undefined

-- El cálculo es

-- ----------------------------------------------------------------------------
-- Ejercicio 24. Definir la función
--   esProductoDeDosPrimos :: Int -> Bool
-- tal que '(esProductoDeDosPrimos n)' se verifica si 'n' es el producto de dos
-- números primos distintos. Por ejemplo,
--   esProductoDeDosPrimos 6  ==  True
--   esProductoDeDosPrimos 9  ==  False
-- ----------------------------------------------------------------------------

esProductoDeDosPrimos :: Int -> Bool
esProductoDeDosPrimos n = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 25. [Problema 37 del proyecto Euler] Un número primo es truncable
-- si los números que se obtienen eliminado cifras, de derecha a izquierda, son
-- números primos. Por ejemplo, 599 es un primo truncable porque 599, 59 y 5
-- son números primos; en cambio, 577 no es un primo truncable porque 57 no es
-- un número primo.
--
-- Definir la función
--   primoTruncable :: Int -> Bool
-- tal que '(primoTruncable x)' se verifica si 'x' es un número primo
-- truncable. Por ejemplo,
--   primoTruncable 599  ==  True
--   primoTruncable 577  ==  False
-- ----------------------------------------------------------------------------

primoTruncable :: Int -> Bool
primoTruncable x = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 26. Definir la función
--   sumaPrimosTruncables :: Int -> Int
-- tal que '(sumaPrimosTruncables n)' es la suma de los 'n' primeros números
-- primos truncables. Por ejemplo,
--   sumaPrimosTruncables 10  ==  249
--
-- Calcular usando esta función la suma de los 20 primeros números primos
-- truncables.
-- ----------------------------------------------------------------------------

sumaPrimosTruncables :: Int -> Int
sumaPrimosTruncables n = undefined

-- El cálculo es

-- ----------------------------------------------------------------------------
-- Ejercicio 27. [Problema 341 del proyecto Euler]. La sucesión de Golomb
-- {G(n)} es una sucesión auto descriptiva: es la única sucesión no decreciente
-- de números naturales tal que el número n aparece G(n) veces en la sucesión.
-- Los valores de G(n) para los primeros números son los siguientes:
--   n       1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 ...
--   G(n)    1 2 2 3 3 4 4 4 5  5  5  6  6  6  6 ...
--
-- Definir la constante
--   sucesionGolomb :: [Int]
-- cuyo valor es la lista infinita de los términos de la sucesión de Golomb.
-- Por ejemplo,
--   take 15 sucesionGolomb  ==  [1,2,2,3,3,4,4,4,5,5,5,6,6,6,6]
-- ----------------------------------------------------------------------------

sucesionGolomb :: [Int]
sucesionGolomb = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 28. [Problema 12 del Proyecto Euler] La sucesión de los números
-- triangulares se obtiene sumando los números naturales. Así, el 7º número
-- triangular es
--   1 + 2 + 3 + 4 + 5 + 6 + 7 = 28.
-- Los primeros 10 números triangulares son
--   1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
-- Los divisores de los primeros 7 números triangulares son:
--    1: 1
--    3: 1,3
--    6: 1,2,3,6
--   10: 1,2,5,10
--   15: 1,3,5,15
--   21: 1,3,7,21
--   28: 1,2,4,7,14,28
-- Como se puede observar, 28 es el menor número triangular con más de 5
-- divisores.
--
-- Definir la función
--   menorTriangularDivisores :: Int -> Int
-- tal que '(menorTriangularDivisores n)' es el menor número triangular con más
-- de 'n' divisores. Por ejemplo,
--   menorTriangularDivisores 5   ==  28
--   menorTriangularDivisores 10  ==  120
--   menorTriangularDivisores 20  ==  630
--   menorTriangularDivisores 50  ==  25200
-- ----------------------------------------------------------------------------

menorTriangularDivisores :: Int -> Int
menorTriangularDivisores n = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 29. Consideramos la secuencia infinita
--   0,0,1,0,2,1,3,0,4,2,5,1,6,3,7,0,8,4,9,2,10,5,11,1,12,6,13,3,14,7,15,...
-- construida de la siguiente forma:
-- * Los términos pares forman la secuencia infinita de los números naturales
--   0,-,1,-,2,-,3,-,4,-,5,-,6,-,7,-,8,-,9,-,10,-,11,-,12,-,13,-,14,-,15,...
-- * Los términos impares forman la misma secuencia infinita original
--   -,0,-,0,-,1,-,0,-,2,-,1,-,3,-,0,-,4,-,2,--,5,--,1,--,6,--,3,--,7,--,...
--
-- Definir la constante
--   secuencia :: [Int]
-- cuyo valor es la secuencia infinita descrita en el enunciado. Por ejemplo,
--   take 10 secuencia  ==  [0,0,1,0,2,1,3,0,4,2]
--   take 20 secuencia  ==  [0,0,1,0,2,1,3,0,4,2,5,1,6,3,7,0,8,4,9,2]
-- ----------------------------------------------------------------------------

secuencia :: [Int]
secuencia = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 30. Definir la función
--   primosSumaIgual :: Int -> [[Int]]
-- tal que '(primosSumaIgual n)' es la lista infinita de las secuencias de 'n'
-- números primos consecutivos tales que la suma de sus cifras es el mismo
-- valor para todos ellos. Por ejemplo,
--   take 2 (primosSumaIgual 2)  ==  [[523,541],[1069,1087]]
--   head (primosSumaIgual 3)    ==  [22193,22229,22247]
-- ----------------------------------------------------------------------------

primosSumaIgual :: Int -> [[Int]]
primosSumaIgual n = undefined

-- ============================================================================
