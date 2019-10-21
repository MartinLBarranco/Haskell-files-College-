-- I1M: Relacion_06.hs
-- Definiciones por comprensión y recursión
-- Departamento de Ciencias de la Computación e Inteligencia Artificial
-- Universidad de Sevilla
-- ============================================================================

-- ============================================================================
-- Librerías auxiliares
-- ============================================================================

import Test.QuickCheck

-- ============================================================================
-- Definiciones por comprensión y recursión
-- ============================================================================

-- ----------------------------------------------------------------------------
-- Ejercicio 1.1. Definir por comprensión la función
--   cuadradosC :: [Int] -> [Int]
-- tal que '(cuadradosC xs)' es la lista de los cuadrados de la lista 'xs'. Por
-- ejemplo,
--   cuadradosC [1,2,3]  ==  [1,4,9]
-- ----------------------------------------------------------------------------

cuadradosC :: [Int] -> [Int]
cuadradosC xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 1.2. Definir por recursión la función
--   cuadradosR :: [Int] -> [Int]
-- tal que '(cuadradosR xs)' es la lista de los cuadrados de la lista 'xs'. Por
-- ejemplo,
--   cuadradosR [1,2,3]  ==  [1,4,9]
-- ----------------------------------------------------------------------------

cuadradosR :: [Int] -> [Int]
cuadradosR xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 1.3. Comprobar con QuickCheck que ambas definiciones son
-- equivalentes.
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_cuadrados :: [Int] -> Bool
prop_cuadrados xs = undefined

-- La comprobación es
--   > quickCheck prop_cuadrados

-- ----------------------------------------------------------------------------
-- Ejercicio 2.1. Definir por comprensión la función
--   imparesC :: [Int] -> [Int]
-- tal que '(imparesC xs)' es la lista de los números impares de la lista 'xs'.
-- Por ejemplo,
--   imparesC [1,2,3]  ==  [1,3]
-- ----------------------------------------------------------------------------

imparesC :: [Int] -> [Int]
imparesC xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 2.2. Definir por recursión la función
--   imparesR :: [Int] -> [Int]
-- tal que '(imparesR xs)' es la lista de los números impares de la lista 'xs'.
-- Por ejemplo,
--   imparesR [1,2,3]  ==  [1,3]
-- ----------------------------------------------------------------------------

imparesR :: [Int] -> [Int]
imparesR xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 2.3. Comprobar con QuickCheck que ambas definiciones son
-- equivalentes.
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_impares :: [Int] -> Bool
prop_impares xs = undefined

-- La comprobación es
--   > quickCheck prop_impares

-- ----------------------------------------------------------------------------
-- Ejercicio 3.1. Definir por comprensión la función
--   imparesCuadradosC :: [Int] -> [Int]
-- tal que '(imparesCuadradosC xs)' es la lista de los cuadrados de los números
-- impares de la lista 'xs'. Por ejemplo,
--   imparesCuadradosC [1,2,3]  ==  [1,9]
-- ----------------------------------------------------------------------------

imparesCuadradosC :: [Int] -> [Int]
imparesCuadradosC xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 3.2. Definir por recursión la función
--   imparesCuadradosR :: [Int] -> [Int]
-- tal que '(imparesCuadradosR xs)' es la lista de los cuadrados de los números
-- impares de la lista 'xs'. Por ejemplo,
--   imparesCuadradosR [1,2,3]  ==  [1,9]
-- ----------------------------------------------------------------------------

imparesCuadradosR :: [Int] -> [Int]
imparesCuadradosR xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 3.3. Comprobar con QuickCheck que ambas definiciones son
-- equivalentes.
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_imparesCuadrados :: [Int] -> Bool
prop_imparesCuadrados xs = undefined

-- La comprobación es
--   > quickCheck prop_imparesCuadrados

-- ----------------------------------------------------------------------------
-- Ejercicio 4.1. Definir por comprensión la función
--   sumaCuadradosImparesC :: [Int] -> Int
-- tal que '(sumaCuadradosImparesC xs)' es la suma de los cuadrados de los
-- números impares de la lista 'xs'. Por ejemplo,
--   sumaCuadradosImparesC [1,2,3]  ==  10
-- ----------------------------------------------------------------------------

sumaCuadradosImparesC :: [Int] -> Int
sumaCuadradosImparesC xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 4.2. Definir por recursión la función
--   sumaCuadradosImparesR :: [Int] -> Int
-- tal que '(sumaCuadradosImparesR xs)' es la suma de los cuadrados de los
-- números impares de la lista 'xs'. Por ejemplo,
--   sumaCuadradosImparesR [1,2,3]  ==  10
-- ----------------------------------------------------------------------------

sumaCuadradosImparesR :: [Int] -> Int
sumaCuadradosImparesR = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 4.3. Comprobar con QuickCheck que ambas definiciones son
-- equivalentes.
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_sumaCuadradosImpares :: [Int] -> Bool
prop_sumaCuadradosImpares xs = undefined

-- La comprobación es
--   > quickCheck prop_sumaCuadradosImpares

-- ----------------------------------------------------------------------------
-- Ejercicio 5.1. Definir por comprensión la función
--   mitadParesC :: [Int] -> [Int]
-- tal que '(mitadParesC xs)' es la lista de las mitades de los elementos pares
-- de la lista 'xs'. Por ejemplo,
--   mitadParesC [0,2,1,7,8,56,17,18]  ==  [0,1,4,28,9]
-- ----------------------------------------------------------------------------

mitadParesC :: [Int] -> [Int]
mitadParesC xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 5.2. Definir por recursión la función
--   mitadParesR :: [Int] -> [Int]
-- tal que '(mitadParesR xs)' es la lista de las mitades de los elementos pares
-- de la lista 'xs'. Por ejemplo,
--   mitadParesR [0,2,1,7,8,56,17,18]  ==  [0,1,4,28,9]
-- ----------------------------------------------------------------------------

mitadParesR :: [Int] -> [Int]
mitadParesR xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 5.3. Comprobar con QuickCheck que ambas definiciones son
-- equivalentes.
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_mitadPares :: [Int] -> Bool
prop_mitadPares xs = undefined

-- La comprobación es
--   > quickCheck prop_mitadPares

-- ----------------------------------------------------------------------------
-- Ejercicio 6.1. Definir por comprensión la función
--   enRangoC :: Int -> Int -> [Int] -> [Int]
-- tal que '(enRangoC a b xs)' es la lista de los elementos de la lista 'xs'
-- que son mayores o iguales que 'a' y menores o iguales que 'b'. Por ejemplo,
--   enRangoC  5 10 [1..15]  ==  [5,6,7,8,9,10]
--   enRangoC 10  5 [1..15]  ==  []
--   enRangoC  5  5 [1..15]  ==  [5]
-- ----------------------------------------------------------------------------

enRangoC :: Int -> Int -> [Int] -> [Int]
enRangoC a b xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 6.2. Definir por recursión la función
--   enRangoR :: Int -> Int -> [Int] -> [Int]
-- tal que '(enRangoC a b xs)' es la lista de los elementos de la lista 'xs'
-- que son mayores o iguales que 'a' y menores o iguales que 'b'. Por ejemplo,
--   enRangoR  5 10 [1..15]  ==  [5,6,7,8,9,10]
--   enRangoR 10  5 [1..15]  ==  []
--   enRangoR  5  5 [1..15]  ==  [5]
-- ----------------------------------------------------------------------------

enRangoR :: Int -> Int -> [Int] -> [Int]
enRangoR a b xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 6.3. Comprobar con QuickCheck que ambas definiciones son
-- equivalentes.
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_enRango :: Int -> Int -> [Int] -> Bool
prop_enRango a b xs = undefined

-- La comprobación es
--   > quickCheck prop_enRango

-- ----------------------------------------------------------------------------
-- Ejercicio 7.1. Definir por comprensión la función
--   sumaPositivosC :: [Int] -> Int
-- tal que '(sumaPositivosC xs)' es la suma de los números positivos de la
-- lista 'xs'. Por ejemplo,
--   sumaPositivosC [0,1,-3,-2,8,-1,6]  ==  15
-- ----------------------------------------------------------------------------

sumaPositivosC :: [Int] -> Int
sumaPositivosC xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 7.2. Definir por recursión la función
--   sumaPositivosR :: [Int] -> Int
-- tal que '(sumaPositivosR xs)' es la suma de los números positivos de la
-- lista 'xs'. Por ejemplo,
--   sumaPositivosR [0,1,-3,-2,8,-1,6]  ==  15
-- ----------------------------------------------------------------------------

sumaPositivosR :: [Int] -> Int
sumaPositivosR xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 7.3. Comprobar con QuickCheck que ambas definiciones son
-- equivalentes.
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_sumaPositivos :: [Int] -> Bool
prop_sumaPositivos xs = undefined

-- La comprobación es
--   > quickCheck prop_sumaPositivos

-- ----------------------------------------------------------------------------
-- Ejercicio 8.1. Definir por comprensión la función
--   sustituyeImparC :: [Int] -> [Int]
-- tal que '(sustituyeImparC xs)' es la lista obtenida sustituyendo cada número
-- impar de la lista 'xs' por el siguiente número par. Por ejemplo,
--   sustituyeImparC [2,5,7,4]  ==  [2,6,8,4]
-- ----------------------------------------------------------------------------

sustituyeImparC :: [Int] -> [Int]
sustituyeImparC xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 8.2. Definir por recursión la función
--   sustituyeImparR :: [Int] -> [Int]
-- tal que '(sustituyeImparR xs)' es la lista obtenida sustituyendo cada número
-- impar de la lista 'xs' por el siguiente número par. Por ejemplo,
--   sustituyeImparR [2,5,7,4]  ==  [2,6,8,4]
-- ----------------------------------------------------------------------------

sustituyeImparR :: [Int] -> [Int]
sustituyeImparR xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 8.3. Comprobar con QuickCheck que ambas definiciones son
-- equivalentes.
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_sustituyeImpar :: [Int] -> Bool
prop_sustituyeImpar xs = undefined

-- La comprobación es
--   > quickCheck prop_sustituyeImpar

-- ----------------------------------------------------------------------------
-- Ejercicio 9.1. Definir por comprensión la función
--   expandeParesC :: [Int] -> [Int]
-- tal que '(expandeParesC xs)' es la lista obtenida a partir de la lista 'xs'
-- repitiendo cada uno de sus elementos pares. Por ejemplo,
--   expandeParesC [3,5,4,6,6,1,0]  ==  [3,5,4,4,6,6,6,6,1,0,0]
--   expandeParesC [3,5,4,6,8,1,0]  ==  [3,5,4,4,6,6,8,8,1,0,0]
-- ----------------------------------------------------------------------------

expandeParesC :: [Int] -> [Int]
expandeParesC xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 9.2. Definir por recursión la función
--   expandeParesR :: [Int] -> [Int]
-- tal que '(expandeParesC xs)' es la lista obtenida a partir de la lista 'xs'
-- repitiendo cada uno de sus elementos pares. Por ejemplo,
--   expandeParesR [3,5,4,6,6,1,0]  ==  [3,5,4,4,6,6,6,6,1,0,0]
--   expandeParesR [3,5,4,6,8,1,0]  ==  [3,5,4,4,6,6,8,8,1,0,0]
-- ----------------------------------------------------------------------------

expandeParesR :: [Int] -> [Int]
expandeParesR xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 9.3. Comprobar con QuickCheck que ambas definiciones son
-- equivalentes.
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_expandePares :: [Int] -> Bool
prop_expandePares xs = undefined

-- La comprobación es
--   > quickCheck prop_expandePares

-- ----------------------------------------------------------------------------
-- Ejercicio 10.1. Una expresión factorizada es una secuencia de pares formados
-- por un número y un exponente. La expansión de una expresión factorizada es
-- el producto de todos los números elevados al exponente que los acompaña. Por
-- ejemplo, [(2,2),(3,1),(5,3)] es una expresión factorizada cuya expansión es
-- 2²*3¹*5³ = 1500
--
-- Definir por comprensión la función
--   expansionC :: [(Int,Int)] -> Int
-- tal que '(expansionC xs)' es la expansión de la expresión factorizada 'xs'.
-- Por ejemplo,
--   expansionC [(2,2),(3,1),(5,3)]  ==  1500
-- ----------------------------------------------------------------------------

expansionC :: [(Int,Int)] -> Int
expansionC xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 10.2. Definir por recursión la función
--   expansionR :: [(Int,Int)] -> Int
-- tal que '(expansionR xs)' es la expansión de la expresión factorizada 'xs'.
-- Por ejemplo,
--   expansionR [(2,2),(3,1),(5,3)]  ==  1500
-- ----------------------------------------------------------------------------

expansionR :: [(Int,Int)] -> Int
expansionR xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 10.3. Comprobar con QuickCheck que ambas definiciones son
-- equivalentes.
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_expansion :: [(Int,Int)] -> Bool
prop_expansion xs = undefined

-- La comprobación es
--   > quickCheck prop_expansion

-- ----------------------------------------------------------------------------
-- Ejercicio 11.1. La distancia Hamming entre dos listas es el número de
-- posiciones en las que los correspondientes elementos son distintos. Por
-- ejemplo, la distancia Hamming entre "roma" y "loba" es 2 (porque hay 2
-- posiciones en las que los elementos correspondientes son distintos: la 1ª y
-- la 3ª).
--
-- Definir por comprensión la función
--   distanciaHammingC :: Eq a => [a] -> [a] -> Int
-- tal que '(distanciaHammingC xs ys)' es la distancia Hamming entre las listas
-- 'xs' e 'ys'. Por ejemplo,
--   distanciaHammingC "romano" "comino"  ==  2
--   distanciaHammingC "romano" "camino"  ==  3
--   distanciaHammingC "roma"   "comino"  ==  2
--   distanciaHammingC "roma"   "camino"  ==  3
--   distanciaHammingC "romano" "ron"     ==  1
--   distanciaHammingC "romano" "cama"    ==  2
--   distanciaHammingC "romano" "rama"    ==  1
-- ----------------------------------------------------------------------------

distanciaHammingC :: Eq a => [a] -> [a] -> Int
distanciaHammingC xs ys = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 11.2. Definir por recursión la función
--   distanciaHammingR :: Eq a => [a] -> [a] -> Int
-- tal que '(distanciaHammingR xs ys)' es la distancia Hamming entre las listas
-- 'xs' e 'ys'. Por ejemplo,
--   distanciaHammingR "romano" "comino"  ==  2
--   distanciaHammingR "romano" "camino"  ==  3
--   distanciaHammingR "roma"   "comino"  ==  2
--   distanciaHammingR "roma"   "camino"  ==  3
--   distanciaHammingR "romano" "ron"     ==  1
--   distanciaHammingR "romano" "cama"    ==  2
--   distanciaHammingR "romano" "rama"    ==  1
-- ----------------------------------------------------------------------------

distanciaHammingR :: Eq a => [a] -> [a] -> Int
distanciaHammingR xs ys = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 11.3. Comprobar con QuickCheck que ambas definiciones son
-- equivalentes.
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_distanciaHamming :: Eq a => [a] -> [a] -> Bool
prop_distanciaHamming xs ys = undefined

-- La comprobación es
--   > quickCheck prop_distanciaHamming

-- ----------------------------------------------------------------------------
-- Ejercicio 12.1. El trozo inicial de los elementos de una lista que cumplen
-- una propiedad es la secuencia de elementos de dicha lista desde la posición
-- 0 hasta el primer elemento que no cumple la propiedad, sin incluirlo. Por
-- ejemplo, el trozo inicial de los elementos de [2,4,3,2] que son pares es
-- [2,4].
--
-- Definir por comprensión la función
--   trozoInicialParesC :: [Int] -> [Int]
-- tal que '(trozoInicialParesC xs)' es el trozo inicial de los elementos de
-- la lista 'xs' que son pares. Por ejemplo,
--   trozoInicialParesC []         ==  []
--   trozoInicialParesC [1,2,3,4]  ==  []
--   trozoInicialParesC [2,4,3,2]  ==  [2,4]
--   trozoInicialParesC [2,4,6,8]  ==  [2,4,6,8]
-- ----------------------------------------------------------------------------

trozoInicialParesC :: [Int] -> [Int]
trozoInicialParesC xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 12.2. Definir por recursión la función
--   trozoInicialParesR :: [Int] -> [Int]
-- tal que '(trozoInicialParesR xs)' es el trozo inicial de los elementos de
-- la lista 'xs' que son pares. Por ejemplo,
--   trozoInicialParesR []         ==  []
--   trozoInicialParesR [1,2,3,4]  ==  []
--   trozoInicialParesR [2,4,3,2]  ==  [2,4]
--   trozoInicialParesR [2,4,6,8]  ==  [2,4,6,8]
-- ----------------------------------------------------------------------------

trozoInicialParesR :: [Int] -> [Int]
trozoInicialParesR xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 12.3. Comprobar con QuickCheck que ambas definiciones son
-- equivalentes.
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_trozoInicialPares :: [Int] -> Bool
prop_trozoInicialPares xs = undefined

-- La comprobación es
--   > quickCheck prop_trozoInicialPares

-- ----------------------------------------------------------------------------
-- Ejercicio 13.1. Una sustitución es una lista de parejas [(x1,y1),..,(xn,yn)]
-- que se usa para indicar que hay que reemplazar cualquier ocurrencia de cada
-- uno de los xi, por el correspondiente yi. Por ejemplo,
--   [('1','a'),('2','n'),('3','v'),('4','i'),('5','d')]
-- es la sustitución que reemplaza '1' por 'a', '2' por 'n', ...
--
-- Definir por comprensión la función
--   sustituyeEltC :: (Eq a) => [(a,a)] -> a -> a
-- tal que '(sustituyeEltC xs z)' es el resultado de aplicar la sustitución
-- 'xs' al elemento 'z'. Por ejemplo,
--   sustituyeEltC sustitucion '4'  ==  'i'
--   sustituyeEltC sustitucion '2'  ==  'n'
--   sustituyeEltC sustitucion '0'  ==  '0'
-- ----------------------------------------------------------------------------

sustitucion = [('1','a'),('2','n'),('3','v'),('4','i'),('5','d')]

sustituyeEltC :: (Eq a) => [(a,a)] -> a -> a
sustituyeEltC xs z = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 13.2. Definir por recursión la función
--   sustituyeEltR :: (Eq a) => [(a,a)] -> a -> a
-- tal que '(sustituyeEltR xs z)' es el resultado de aplicar la sustitución
-- 'xs' al elemento 'z'. Por ejemplo,
--   sustituyeEltR sustitucion '4'  ==  'i'
--   sustituyeEltR sustitucion '2'  ==  'n'
--   sustituyeEltR sustitucion '0'  ==  '0'
-- ----------------------------------------------------------------------------

sustituyeEltR :: (Eq a) => [(a,a)] -> a -> a
sustituyeEltR xs z = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 13.3. Comprobar con QuickCheck que ambas definiciones son
-- equivalentes.
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_sustituyeElt :: (Eq a) => [(a,a)] -> a -> Bool
prop_sustituyeElt xs z = undefined

-- La comprobación es
--   > quickCheck prop_sustituyeElt

-- ----------------------------------------------------------------------------
-- Ejercicio 14.1. Definir por comprensión la función
--   sustituyeLstC :: (Eq a) => [(a,a)] -> [a] -> [a]
-- tal que '(sustituyeLstC xs zs)' es el resultado de aplicar la sustitución
-- 'xs' a los elementos de la lista 'zs'. Por ejemplo,
--   sustituyeLstC sustitucion "2151"     ==  "nada"
--   sustituyeLstC sustitucion "3451"     ==  "vida"
--   sustituyeLstC sustitucion "2134515"  ==  "navidad"
-- ----------------------------------------------------------------------------

sustituyeLstC :: (Eq a) => [(a,a)] -> [a] -> [a]
sustituyeLstC xs zs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 14.2. Definir por recursión la función
--   sustituyeLstR :: (Eq a) => [(a,a)] -> [a] -> [a]
-- tal que '(sustituyeLstR xs zs)' es el resultado de aplicar la sustitución
-- 'xs' a los elementos de la lista 'zs'. Por ejemplo,
--   sustituyeLstR sustitucion "2151"     ==  "nada"
--   sustituyeLstR sustitucion "3451"     ==  "vida"
--   sustituyeLstR sustitucion "2134515"  ==  "navidad"
-- ----------------------------------------------------------------------------

sustituyeLstR :: (Eq a) => [(a,a)] -> [a] -> [a]
sustituyeLstR xs zs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 14.3. Comprobar con QuickCheck que ambas definiciones son
-- equivalentes.
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_sustituyeLst :: (Eq a) => [(a,a)] -> [a] -> Bool
prop_sustituyeLst xs zs = undefined

-- La comprobación es
--   > quickCheck prop_sustituyeLst

-- ----------------------------------------------------------------------------
-- Ejercicio 15.1. Definir por comprensión la función
--   superparC :: Int -> Bool
-- tal que '(superparC n)' se verifica si 'n' es un número entero tal que todos
-- sus dígitos son pares. Por ejemplo,
--   superparC 426   ==  True
--   superparC 456   ==  False
--   superparC (-2)  ==  True
-- ----------------------------------------------------------------------------

superparC :: Int -> Bool
superparC n = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 15.2. Definir por recursión la función
--   superparR :: Int -> Bool
-- tal que '(superparR n)' se verifica si 'n' es un número entero tal que todos
-- sus dígitos son pares. Por ejemplo,
--   superparR 426   ==  True
--   superparR 456   ==  False
--   superparR (-2)  ==  True
-- ----------------------------------------------------------------------------

superparR :: Int -> Bool
superparR n = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 15.3. Comprobar con QuickCheck que ambas definiciones son
-- equivalentes.
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_superpar :: Int -> Bool
prop_superpar n = undefined

-- La comprobación es
--   > quickCheck prop_superpar

-- ============================================================================
