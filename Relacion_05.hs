-- I1M: Relacion_05.hs
-- Definiciones por recursión
-- Departamento de Ciencias de la Computación e Inteligencia Artificial
-- Universidad de Sevilla
-- ============================================================================

-- ============================================================================
-- Recursión sobre números
-- ============================================================================

-- ----------------------------------------------------------------------------
-- Ejercicio 1. Definir por recursión la función
--   factorial :: Integer -> Integer
-- tal que '(factorial n)' es el factorial del número natural 'n'. Por ejemplo,
--   factorial 0   ==  1
--   factorial 1   ==  1
--   factorial 5   ==  120
--   factorial 10  ==  3628800
--   factorial 20  ==  2432902008176640000
-- ----------------------------------------------------------------------------

factorial :: Integer -> Integer
factorial n = foldr (*) 1 [1..n]

-- ----------------------------------------------------------------------------
-- Ejercicio 2. Definir por recursión la función
--   calculaPi :: Double -> Double
-- tal que '(calculaPi n)' es la aproximación del número π calculada mediante
-- la expresión '4*(1 - 1/3 + 1/5 - 1/7 + ... + (-1)**n/(2*n+1))'. Por ejemplo,
--   calculaPi 3    ==  2.8952380952380956
--   calculaPi 300  ==  3.1449149035588526
-- ----------------------------------------------------------------------------

calculaPi :: Double -> Double
calculaPi n = 4*sum[((-1)**n) / (2*i+1) | i<-[1..n]]

-- ----------------------------------------------------------------------------
-- Ejercicio 3. Definir por recursión la función
--   digitos :: Integer -> [Integer]
-- tal que '(digitos n)' es la lista de los dígitos del número natural 'n'. Por
-- ejemplo,
--   digitos 0       ==  [0]
--   digitos 320274  ==  [3,2,0,2,7,4]
-- ----------------------------------------------------------------------------

digitos :: Integer -> [Integer]
digitos n =
  let c = div n 10
      r = mod n 10
  in (digitos c) ++ [r]
-- ----------------------------------------------------------------------------
-- Ejercicio 4. El doble factorial de un número natural n se define por
--   n!! = n*(n-2)* ... * 3 * 1, si n es impar
--   n!! = n*(n-2)* ... * 4 * 2, si n es par
--   1!! = 1
--   0!! = 1
-- Por ejemplo,
--   8!! = 8*6*4*2   = 384
--   9!! = 9*7*5*3*1 = 945
--
-- Definir por recursión la función
--   dobleFactorial :: Integer -> Integer
-- tal que '(dobleFactorial n)' es el doble factorial del número natural 'n'.
-- Por ejemplo,
--   dobleFactorial 8  ==  384
--   dobleFactorial 9  ==  945
-- ----------------------------------------------------------------------------

dobleFactorial :: Integer -> Integer
dobleFactorial n | even n = product(filter (even ) [1..n])
                 | otherwise = product(filter (odd ) [1..n])

-- ----------------------------------------------------------------------------
-- Ejercicio 5. La suma de la serie
--   1/1^2 + 1/2^2 + 1/3^2 + 1/4^2 + ...
-- es π²/6. Por tanto, π se puede aproximar mediante la raíz cuadrada del
-- producto de 6 por dicha suma.
--
-- Definir por recursión la función
--   calculaPi2 :: Double -> Double
-- tal que '(calculaPi2 n)' es la aproximación del número π obtenida mediante
-- 'n' términos de la serie anterior. Por ejemplo,
--   calculaPi2 10     ==  3.0493616359820703
--   calculaPi2 100    ==  3.1320765318091075
--   calculaPi2 1000   ==  3.140638056205994
--   calculaPi2 10000  ==  3.1414971639472378
-- ----------------------------------------------------------------------------

calculaPi2 :: Double -> Double
calculaPi2 n =  sqrt(6*sumaD n)
                where sumaD :: Double -> Double
                      sumaD 1 = 1
                      sumaD n  = n**(-2) + sumaD (n-1)

-- ----------------------------------------------------------------------------
-- Ejercicio 6. Dados dos números naturales, A y B, es posible calcular su
-- máximo común divisor mediante el Algoritmo de Euclides. Este algoritmo se
-- puede resumir en la siguiente fórmula:
--   mcd(A,B) = A,                   si B = 0
--            = mcd (B, A módulo B), si B > 0
--
-- Definir por recursión la función
--   mcd :: Integer -> Integer -> Integer
-- tal que '(mcd a b)' es el máximo común divisor de los números naturales 'a'
-- y 'b' calculado mediante el algoritmo de Euclides. Por ejemplo,
--   mcd 30 45  ==  15
-- ----------------------------------------------------------------------------

mcd :: Integer -> Integer -> Integer
mcd a b |a == b = a
        |a > b = mcd1 a b
        |otherwise = mcd1 b a

mcd1 :: Integer -> Integer -> Integer
mcd1 a b |a `mod` b  == 0 = b        
         |otherwise = mcd1 b (a `mod` b)

-- ----------------------------------------------------------------------------
-- Ejercicio 7. Definir por recursión la función
--   menorDivisible :: Integer -> Integer -> Integer
-- tal que '(menorDivisible a b)' es el menor número natural divisible por
-- todos los números desde el número 'a' hasta el número 'b'. Por ejemplo,
--   menorDivisible 2 5  ==  60
-- ----------------------------------------------------------------------------
menorDivisible :: Integer -> Integer -> Integer
menorDivisible a b = (factorial a) `div` (factorial b)
                           
-- ----------------------------------------------------------------------------
-- Ejercicio 8. En un templo hindú se encuentran tres varillas de platino. En
-- una de ellas, hay 64 anillos de oro de distintos radios, colocados de mayor
-- a menor.
--
-- El trabajo de los monjes de ese templo consiste en pasarlos todos a la
-- tercera varilla, usando la segunda como varilla auxiliar, con las siguientes
-- condiciones:
--   * En cada paso sólo se puede mover un anillo.
--   * Nunca puede haber un anillo de mayor diámetro encima de uno de menor
--     diámetro.
-- La leyenda dice que cuando todos los anillos se encuentren en la tercera
-- varilla, llegará el fin del mundo.
--
-- Definir por recursión la función
--   numPasosHanoi :: Integer -> Integer
-- tal que '(numPasosHanoi n)' es el número de pasos necesarios para trasladar
-- 'n' anillos. Por ejemplo,
--   numPasosHanoi 2   ==  3
--   numPasosHanoi 7   ==  127
--   numPasosHanoi 64  ==  18446744073709551615
-- ----------------------------------------------------------------------------

-- Sean A, B y C las tres varillas. La estrategia recursiva es la siguiente:
-- * Caso base (N=1): Se mueve el disco de A a C.
-- * Caso inductivo (N=M+1): Se mueven M discos de A a C. Se mueve el disco
--   de A a B. Se mueven M discos de C a B.
-- Por tanto,

numPasosHanoi :: Integer -> Integer
numPasosHanoi n = (2^n) -1

-- ----------------------------------------------------------------------------
-- Ejercicio 9. La sucesión de Fibonacci es
--   1, 1, 2, 3, 5, 8, 13, ...
-- es decir, los dos primeros valores son 1 y a partir de ahí cualquier término
-- de la sucesión es igual a la suma de los dos anteriores.
--
-- Definir por recursión la función
--   fibonacci :: Integer -> Integer
-- tal que '(fibonacci n)' es el 'n'-ésimo término de la sucesión de Fibonacci.
-- Por ejemplo,
--   fibonacci 1   ==  1
--   fibonacci 5   ==  5
--   fibonacci 10  ==  55
-- ----------------------------------------------------------------------------

fibonacci :: Integer -> Integer
fibonacci 1 = 1
fibonacci 2 = 1
fibonnaci n = fibonnaci (n-2) + fibonnaci (n-1)

-- ----------------------------------------------------------------------------
-- Ejercicio 10. Definir por recursión la función
--   pegaNumeros :: Integer -> Integer -> Integer
-- tal que '(pegaNumeros n m)' es el número resultante de "pegar" los números
-- 'n' y 'm'. Por ejemplo,
--   pegaNumeros 12 987   ==  12987
--   pegaNumeros 1204 7   ==  12047
--   pegaNumeros 100 100  ==  100100
-- ----------------------------------------------------------------------------

pegaNumeros :: Integer -> Integer -> Integer
pegaNumeros x y = read(show x ++ show y)

-- ----------------------------------------------------------------------------
-- Ejercicio 11. La exponenciación binaria es un algoritmo utilizado para
-- calcular de forma rápida grandes potencias enteras de un número x. El
-- algoritmo recursivo es el siguiente:
--   * si n = 1, entonces x^1 = x
--   * si n es par, entonces x^n = (x^2)^(n/2)
--   * si n es impar, entonces x^n = x*x^(n-1)
--
-- Definir por recursión la función
--   potenciaBinaria :: Integer -> Integer -> Integer
-- tal que '(potenciaBinaria x n)' es 'x^n', calculado según el algoritmo
-- anterior. Por ejemplo,
--   potenciaBinaria 3 5   ==  243
--   potenciaBinaria 5 17  ==  762939453125
-- ----------------------------------------------------------------------------

potenciaBinaria :: Integer -> Integer -> Integer
potenciaBinaria x n = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 12. El método ruso para multiplicar dos números A y B consiste en:
--   * Escribir los números A y B en sendas columnas.
--   * Dividir A entre 2, sucesivamente, ignorando el resto, hasta llegar a la
--     unidad. Escribir los resultados en la columna A.
--   * Multiplicar B por 2 tantas veces como veces se ha dividido A entre 2.
--     Escribir los resultados sucesivos en la columna B.
--   * Sumar todos los números de la columna B que estén al lado de un número
--     impar de la columna A. Éste es el resultado.
--
-- Ejemplo: 27 × 82
--           A   B    Sumandos
--          27   82     82
--          13  164    164
--           6  328
--           3  656    656
--           1  1312  1312
-- Resultado:         2214
--
-- Definir por recursión la función
--   multiplicacionRusa :: Integer -> Integer -> Integer
-- tal que '(multiplicacionRusa a b)' es el resultado de aplicar el algoritmo
-- anterior a los números naturales 'a' y 'b'. Por ejemplo,
--   multiplicacionRusa 27 82   ==  2214
--   multiplicacionRusa 12 124  ==  1488
--   multiplicacionRusa 24 203  ==  4872
-- ----------------------------------------------------------------------------

multiplicacionRusa :: Integer -> Integer -> Integer
multiplicacionRusa 1 b = b
multiplicacionRusa a b | odd a = b + multiplicacionRusa (a `div` 2) b*2
                       | otherwise = multiplicacionRusa (a `div` 2) b

-- ----------------------------------------------------------------------------
-- Ejercicio 13. Definir por recursión la función
--   numeroDigitosPares :: Integer -> Integer
-- tal que '(numeroDigitosPares n)' es la cantidad de dígitos pares que hay en
-- el número natural 'n'. Por ejemplo,
--   numeroDigitosPares      0  ==  1
--   numeroDigitosPares      1  ==  0
--   numeroDigitosPares    246  ==  3
--   numeroDigitosPares    135  ==  0
--   numeroDigitosPares 123456  ==  3
-- ----------------------------------------------------------------------------

numeroDigitosPares :: Integer -> Int
numeroDigitosPares n = length(filter (even ) (digitos n))

-- ----------------------------------------------------------------------------
-- Ejercicio 14. Definir por recursión la función
--   ceros :: Integer -> Integer
-- tal que '(ceros n)' es el número de ceros en los que termina el número 'n'.
-- Por ejemplo,
--   ceros 3020000  ==  4
-- ----------------------------------------------------------------------------

ceros :: Integer -> Int
ceros n = length(filter ( == 0) (digitos n))

-- ----------------------------------------------------------------------------
-- Ejercicio 15. El algoritmo para expresar un número N en base B consiste en
-- dividir N, y todos los cocientes que se vayan obteniendo, por B hasta
-- llegar a 0 y entonces considerar el número formado por los restos de cada
-- división realizada, en orden inverso. Por ejemplo, para expresar el número
-- 123 en base 3:
--               cociente     resto
--     123 : 3      41          0
--      41 : 3      13          2
--      13 : 3       4          1
--       4 : 3       1          1
--       1 : 3       0          1
--
-- El número 123 expresado en base 3 es 11120. En efecto:
--   123 = 1*3^4 + 1*3^3 + 1*3^2 + 2*3^1 + 0*3^0
--
-- Definir por recursión la función
--   cambioBase :: Integer -> Integer -> Integer
-- tal que '(cambioBase n b)' es la expresión del número 'n' en base 'b'. Por
-- ejemplo,
--   cambioBase 123 2   ==  1111011
--   cambioBase 123 3   ==  11120
--   cambioBase 123 5   ==  443
--   cambioBase 123 10  ==  123
-- ----------------------------------------------------------------------------

cambioBase :: Integer -> Integer -> [Integer]    --HE TENIDO QUE PONERLO COMO LISTA
cambioBase n b = cambioBase (n`div`b) b ++ [n `mod` b]   --PONER UNA INVERSA DE DIGITOS

-- ============================================================================
-- Recursión sobre listas
-- ============================================================================

-- ----------------------------------------------------------------------------
-- Ejercicio 16. Definir por recursión la función
--   listaMenoresIguales :: Ord a => a -> [a] -> [a]
-- tal que '(listaMenoresIguales x ys)' es la lista de los elementos de la
-- lista 'ys' menores o iguales que 'x'. Por ejemplo,
--   listaMenoresIguales 5 [1,3,5,7,9]  ==  [1,3,5]
-- ----------------------------------------------------------------------------

listaMenoresIguales :: Ord a => a -> [a] -> [a]
listaMenoresIguales _ [] = []
listaMenoresIguales x (y:ys) |y<=x = y : listaMenoresIguales x ys
                             |otherwise = listaMenoresIguales x ys
-- ----------------------------------------------------------------------------
-- Ejercicio 17. Definir por recursión la función
--   listaMayores :: Ord a => a -> [a] -> [a]
-- tal que '(listaMayores x ys)' es la lista de los elementos de la lista 'ys'
-- mayores que 'x'. Por ejemplo,
--   listaMayores 5 [1,3,5,7,9]  ==  [7,9]
-- ----------------------------------------------------------------------------

listaMayores :: Ord a => a -> [a] -> [a]
listaMayores _ [] = []
listaMayores x (y:ys) |y>x = y : listaMayores x ys
                      |otherwise = listaMayores x ys

-- ----------------------------------------------------------------------------
-- Ejercicio 18. Definir por recursión la función
--   elimina :: Eq a => a -> [a] -> [a]
-- tal que '(elimina x ys)' es la lista obtenida eliminando de la lista 'ys'
-- todas las ocurrencias del elemento 'x'. Por ejemplo,
--   elimina 1 [1,2,1]  ==  [2]
--   elimina 3 [1,2,1]  ==  [1,2,1]
-- ----------------------------------------------------------------------------

elimina :: Eq a => a -> [a] -> [a]
elimina _ [] = []
elimina x (y:ys) | y == x = elimina x ys
                 |otherwise = x : elimina x ys
  
-- ----------------------------------------------------------------------------
-- Ejercicio 19. Definir por recursión la función
--   eliminaUna :: Eq a => a -> [a] -> [a]
-- tal que '(eliminaUna x ys)' es la lista obtenida eliminando de la lista 'ys'
-- la primera ocurrencia del elemento 'x'. Por ejemplo,
--   eliminaUna 1 [1,2,1]  ==  [2,1]
--   eliminaUna 3 [1,2,1]  ==  [1,2,1]
-- ----------------------------------------------------------------------------

eliminaUna :: Eq a => a -> [a] -> [a]
eliminaUna _ [] = []
eliminaUna x (y:ys) | x == y = ys
                    | otherwise = x : elimina x ys

-- ----------------------------------------------------------------------------
-- Ejercicio 20. Definir por recursión la función
--   esPermutacion :: Eq a => [a] -> [a] -> Bool
-- tal que '(esPermutacion xs ys)' se verifica si la lista 'xs' es una
-- permutación de la lista 'ys'. Por ejemplo,
--   esPermutacion [1,2,1] [2,1,1]  ==  True
--   esPermutacion [1,2,1] [1,2,2]  ==  False
-- ----------------------------------------------------------------------------

esPermutacion :: Eq a => [a] -> [a] -> Bool
esPermutacion (x:xs) ys = elem x ys && esPermutacion xs ys

-- ----------------------------------------------------------------------------
-- Ejercicio 21. Definir por recursión la función
--   ordenada :: Ord a => [a] -> Bool
-- tal que '(ordenada xs)' se verifica si la lista 'xs' está ordenada en orden
-- creciente. Por ejemplo,
--   ordenada [2,3,5]  ==  True
--   ordenada [2,5,3]  ==  False
-- ----------------------------------------------------------------------------

ordenada :: Ord a => [a] -> Bool
ordenada [] = True
ordenada [x] = True
ordenada (x:xs) = x < head xs && ordenada xs 

-- ----------------------------------------------------------------------------
-- Ejercicio 22. Una lista hermanada es una lista de números estrictamente
-- positivos en la que cada elemento tiene algún factor primo en común con el
-- siguiente, en caso de que exista, o alguno de los dos es un 1. Por ejemplo,
-- · [2,6,3,9,1,5] es una lista hermanada pues 2 y 6 tienen un factor en común
--   (2); 6 y 3 tienen un factor en común (3); 3 y 9 tienen un factor en común
--   (3); de 9 y 1 uno es el número 1; y de 1 y 5 uno es el número 1.
-- · [2,3,5] no es una lista hermanada pues 2 y 3 no tienen ningún factor primo
--   en común.
--
-- Definir por recursión la función
--   hermanada :: [Int] -> Bool
-- tal que '(hermanada xs)' se verifica si la lista 'xs' es hermanada según la
-- definición anterior. Por ejemplo,
--   hermanada [2,6,3,9,1,5]  ==  True
--   hermanada [2,3,5]        ==  False
-- ----------------------------------------------------------------------------

hermanada :: [Int] -> Bool
hermanada [] = True
hermanada (x:y:xs) = x`mod`y == 0 && hermanada (y:xs)

-- ----------------------------------------------------------------------------
-- Ejercicio 23. Definir por recursión la función
--   refinada :: [Float] -> [Float]
-- tal que '(refinada xs)' es la lista obtenida intercalando entre cada dos
-- elementos consecutivos de la lista 'xs' su media aritmética. Por ejemplo,
--   refinada [2,7,1,8]  ==  [2.0,4.5,7.0,4.0,1.0,4.5,8.0]
--   refinada [2]        ==  [2.0]
--   refinada []         ==  []
-- ----------------------------------------------------------------------------

refinada :: [Float] -> [Float]
refinada [] = []
refinada [x,y] = [x, (x+y)/2, y]
refinada (x:y:xs) = [x, (x+y)/2 ] ++ refinada (y:xs)

-- ----------------------------------------------------------------------------
-- Ejercicio 24. Definir por recursión la función
--   mezcla :: Ord a => [a] -> [a] -> [a]
-- tal que '(mezcla xs ys)' es la lista obtenida mezclando las listas ordenadas
-- 'xs' e 'ys', de forma que el resultado quede ordenado. Por ejemplo,
--   mezcla [2,5,6] [1,3,4]  ==  [1,2,3,4,5,6]
-- ----------------------------------------------------------------------------
ordena :: Ord a => [a] -> [a]
ordena [] = []
ordena [x] = [x]
ordena (x:y:xs) |x<=y = [x] ++ ordena (y:xs)
                |otherwise = ordena (y:x:xs)

mezcla :: Ord a => [a] -> [a] -> [a]
mezcla [] _ = []
mezcla _ [] = []
mezcla xs ys = ordena (xs++ys)


-- ----------------------------------------------------------------------------
-- Ejercicio 25. Definir por recursión la función
--   mitadPares :: Ord a => [a] -> [a]
-- tal que '(mitadPares xs)' es la lista de los elementos de la lista 'xs' que
-- ocupan una posición par. Por ejemplo,
--   mitadPares [2,5,6]    ==  [2,6]
--   mitadPares [1,2,3,4]  ==  [1,3]
-- ----------------------------------------------------------------------------

mitadPares :: Ord a => [a] -> [a]
mitadPares [] = []
mitadesPares [x] = []
mitadesPares (x:xs) = head xs : mitadesPares xs 

-- ----------------------------------------------------------------------------
-- Ejercicio 26. Definir por recursión la función
--   mitadImpares :: Ord a => [a] -> [a]
-- tal que '(mitadImpares xs)' es la lista de los elementos de la lista 'xs'
-- que ocupan una posición impar. Por ejemplo,
--   mitadImpares [2,5,6]    ==  [5]
--   mitadImpares [1,2,3,4]  ==  [2,4]
-- ----------------------------------------------------------------------------

mitadImpares :: Ord a => [a] -> [a]
mitadImpares [] = []
mitadImpares [x] = [x]
mitdImpares (x:xs) = [x] ++ mitadImpares (tail xs)

-- ----------------------------------------------------------------------------
-- Ejercicio 27. Definir por recursión la función
--   ordenaMezcla :: Ord a => [a] -> [a]
-- tal que '(ordenaMezcla xs)' es la lista obtenida ordenado 'xs' por mezcla,
-- es decir, considerando que la lista vacía y las listas unitarias están
-- ordenadas y cualquier otra lista se ordena mezclando las dos listas que
-- resultan de ordenar sus dos mitades (pares e impares) por separado. Por
-- ejemplo,
--   ordenaMezcla [5,2,3,1,7,2,5]  ==  [1,2,2,3,5,5,7]
-- ----------------------------------------------------------------------------

ordenaMezcla :: Ord a => [a] -> [a]
ordenaMezcla xs = undefined    --No entiendo lo que me pide

-- ----------------------------------------------------------------------------
-- Ejercicio 28. Definir por recursión la función
--   numeroConsecutivos :: (Num a, Eq a) => [a] -> Int
-- tal que '(numeroConsecutivos xs)' es la cantidad de números consecutivos
-- que aparecen al comienzo de la lista 'xs'. Por ejemplo,
--   numeroConsecutivos [1,3,5,7,9]      ==  1
--   numeroConsecutivos [1,2,3,4,5,7,9]  ==  5
--   numeroConsecutivos [1,2,3,5,6,7]    ==  3
--   numeroConsecutivos []               ==  0
-- ----------------------------------------------------------------------------

numeroConsecutivos :: (Num a, Eq a) => [a] -> Int
numeroConsecutivos [] = 0
numeroConsecutivos [x] = 1
numeroConsecutivos [x,y] | y==x+1 = 1
                         | otherwise = 0
numeroConsecutivos(x:y:xs) | y==x+1 = 1 + numeroConsecutivos (y:xs)
                           |otherwise = numeroConsecutivos (y:xs)

-- ----------------------------------------------------------------------------
-- Ejercicio 29. [De la IMO 1996] Una lista [a(0),a(1),...,a(n)] se denomina
-- cuadrática si para cada i ∈ {1, 2,..., n} se cumple que
--   |a(i) − a(i−1)| = i^2
--
-- Definir por recursión la función
--   esCuadratica :: [Int] -> Bool
-- tal que '(esCuadratica xs)' se verifica si la lista 'xs' es cuadrática. Por
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
-- Definir por recursión la función
--   encadenado :: [Int] -> Bool
-- tal que '(encadenado xs)' se verifica si 'xs' es una lista de números
-- positivos encadenados. Por ejemplo,
--   encadenado [711,1024,413,367]  ==  True
--   encadenado [711,1024,213,367]  ==  False
-- ----------------------------------------------------------------------------

encadenado :: [Int] -> Bool
encadenado (x:xs) = undefined
                  
-- ============================================================================
