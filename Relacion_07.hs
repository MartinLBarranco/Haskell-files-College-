-- I1M: Relacion_07.hs
-- Funciones de orden superior y definiciones por plegados (I)
-- Departamento de Ciencias de la Computación e Inteligencia Artificial
-- Universidad de Sevilla
-- ============================================================================

-- ============================================================================
-- Librerías auxiliares
-- ============================================================================

import Test.QuickCheck

-- ============================================================================
-- Funciones de orden superior
-- ============================================================================

-- ----------------------------------------------------------------------------
-- Ejercicio 1. Definir usando funciones de orden superior la función
--   cuadradosS :: [Int] -> [Int]
-- tal que '(cuadradosS xs)' es la lista de los cuadrados de la lista 'xs'. Por
-- ejemplo,
--   cuadradosS [1,2,3]  ==  [1,4,9]
-- ----------------------------------------------------------------------------

cuadradosS :: [Int] -> [Int]
cuadradosS xs = map ( ^2) xs

-- ----------------------------------------------------------------------------
-- Ejercicio 2. Definir usando funciones de orden superior la función
--   imparesS :: [Int] -> [Int]
-- tal que '(imparesS xs)' es la lista de los números impares de la lista 'xs'.
-- Por ejemplo,
--   imparesS [1,2,3]  ==  [1,3]
-- ----------------------------------------------------------------------------

imparesS :: [Int] -> [Int]
imparesS xs = filter (odd ) xs

-- ----------------------------------------------------------------------------
-- Ejercicio 3. Definir usando funciones de orden superior la función
--   sumaCuadradosImparesS :: [Int] -> Int
-- tal que '(sumaCuadradosImpares xs)' es la suma de los cuadrados de los
-- números impares de la lista 'xs'. Por ejemplo,
--   sumaCuadradosImparesS [1,2,3]  ==  10
-- ----------------------------------------------------------------------------

sumaCuadradosImparesS :: [Int] -> Int
sumaCuadradosImparesS xs = sum (cuadradosS (imparesS xs))


-- ----------------------------------------------------------------------------
-- Ejercicio 4. Definir usando funciones de orden superior la función
--   enRangoS :: Int -> Int -> [Int] -> [Int]
-- tal que '(enRangoS a b xs)' es la lista de los elementos de la lista 'xs'
-- que son mayores o iguales que 'a' y menores o iguales que 'b'. Por ejemplo,
--   enRangoS  5 10 [1..15]  ==  [5,6,7,8,9,10]
--   enRangoS 10  5 [1..15]  ==  []
--   enRangoS  5  5 [1..15]  ==  [5]
-- ----------------------------------------------------------------------------

enRangoS :: Int -> Int -> [Int] -> [Int]
enRangoS a b xs = filter ( <=b) (filter ( >=a) xs)

-- ----------------------------------------------------------------------------
-- Ejercicio 5. Definir usando funciones de orden superior la función
--   sustituyeImparS :: [Int] -> [Int]
-- tal que '(sustituyeImparS xs)' es la lista obtenida sustituyendo cada número
-- impar de la lista 'xs' por el siguiente número par. Por ejemplo,
--   sustituyeImparS [2,5,7,4]  ==  [2,6,8,4]
-- ----------------------------------------------------------------------------
pasaPar :: Int -> Int
pasaPar n | odd n = n+1
          |otherwise = n


sustituyeImparS :: [Int] -> [Int]
sustituyeImparS xs = map (pasaPar ) xs

-- ----------------------------------------------------------------------------
-- Ejercicio 6. Definir usando funciones de orden superior la función
--   expandeParesS :: [Int] -> [Int]
-- tal que '(expandeParesS xs)' es la lista obtenida a partir de la lista 'xs'
-- repitiendo cada uno de sus elementos pares. Por ejemplo,
--   expandeParesS [3,5,4,6,6,1,0]  ==  [3,5,4,4,6,6,6,6,1,0,0]
--   expandeParesS [3,5,4,6,8,1,0]  ==  [3,5,4,4,6,6,8,8,1,0,0]
-- ----------------------------------------------------------------------------

expandeParesS :: [Int] -> [Int]
expandeParesS [] = []
expandeParesS (x:xs) | even x = [x,x] ++ expandeParesS xs
                     |otherwise = expandeParesS xs

-- ----------------------------------------------------------------------------
-- Ejercicio 7. Una expresión factorizada es una secuencia de pares formados
-- por un número y un exponente. La expansión de una expresión factorizada es
-- el producto de todos los números elevados al exponente que los acompaña. Por
-- ejemplo [(2,2),(3,1),(5,3)] es una expresión factorizada cuya expansión es
-- 2²*3¹*5³ = 1500
--
-- Definir usando funciones de orden superior la función
--   expansionS :: [(Int,Int)] -> Int
-- tal que '(expansionS xs)' es la expansión de la expresión factorizada 'xs'.
-- Por ejemplo,
--   expansionS [(2,2),(3,1),(5,3)]  ==  1500
-- ----------------------------------------------------------------------------

eleva :: (Int,Int) -> Int
eleva (a,b) = a^b

expansionS :: [(Int,Int)] -> Int
expansionS xs = product(map (eleva ) xs)

-- ----------------------------------------------------------------------------
-- Ejercicio 8. La distancia Hamming entre dos listas es el número de
-- posiciones en las que los correspondientes elementos son distintos. Por
-- ejemplo, la distancia Hamming entre "roma" y "loba" es 2 (porque hay 2
-- posiciones en las que los elementos correspondientes son distintos: la 1ª y
-- la 3ª).
--
-- Definir usando funciones de orden superior la función
--   distanciaHammingS :: Eq a => [a] -> [a] -> Int
-- tal que '(distanciaHammingS xs ys)' es la distancia Hamming entre las listas
-- 'xs' e 'ys'. Por ejemplo,
--   distanciaHammingS "romano" "comino"  ==  2
--   distanciaHammingS "romano" "camino"  ==  3
--   distanciaHammingS "roma"   "comino"  ==  2
--   distanciaHammingS "roma"   "camino"  ==  3
--   distanciaHammingS "romano" "ron"     ==  1
--   distanciaHammingS "romano" "cama"    ==  2
--   distanciaHammingS "romano" "rama"    ==  1
-- ----------------------------------------------------------------------------
evalua :: Eq a => [(a,a)] -> Int
evalua [] = 0
evalua (x:xs) | fst x /= snd x = 1+evalua xs
              |otherwise = evalua xs

distanciaHammingS :: Eq a => [a] -> [a] -> Int
distanciaHammingS xs ys = evalua(zip xs ys)

-- ----------------------------------------------------------------------------
-- Ejercicio 9. El trozo inicial de los elementos de una lista que cumplen
-- una propiedad es la secuencia de elementos de dicha lista desde la posición
-- 0 hasta el primer elemento que no cumple la propiedad, sin incluirlo. Por
-- ejemplo, el trozo inicial de los elementos de [2,4,3,2] que son pares es
-- [2,4].
--
-- Definir usando funciones de orden superior la función
--   trozoInicialParesS :: [Int] -> [Int]
-- tal que '(trozoInicialPares xs)' es el trozo inicial de los elementos de
-- la lista 'xs' que son pares. Por ejemplo,
--   trozoInicialParesS []         ==  []
--   trozoInicialParesS [1,2,3,4]  ==  []
--   trozoInicialParesS [2,4,3,2]  ==  [2,4]
--   trozoInicialParesS [2,4,6,8]  ==  [2,4,6,8]
-- ----------------------------------------------------------------------------

trozoInicialParesS :: [Int] -> [Int]
trozoInicialParesS xs = takeWhile even xs

-- ----------------------------------------------------------------------------
-- Ejercicio 10. Una sustitución es una lista de parejas [(x1,y1),...,(xn,yn)]
-- que se usa para indicar que hay que reemplazar cualquier ocurrencia de cada
-- uno de los xi, por el correspondiente yi. Por ejemplo,
--   [('1','a'),('2','n'),('3','v'),('4','i'),('5','d')]
-- es la sustitución que reemplaza '1' por 'a', '2' por 'n', ...
--
-- Definir usando funciones de orden superior la función
--   sustituyeEltS :: (Eq a) => [(a,a)] -> a -> a
-- tal que '(sustituyeEltS xs z)' es el resultado de aplicar la sustitución
-- 'xs' al elemento 'z'. Por ejemplo,
--   sustituyeEltS sustitucion '4'  ==  'i'
--   sustituyeEltS sustitucion '2'  ==  'n'
--   sustituyeEltS sustitucion '0'  ==  '0'
-- ----------------------------------------------------------------------------

sustitucion = [('1','a'),('2','n'),('3','v'),('4','i'),('5','d')]

sustituyeEltS :: (Eq a) => [(a,a)] -> a -> a
sustituyeEltS xs z = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 11. Definir usando funciones de orden superior la función
--   sustituyeLstS :: (Eq a) => [(a,a)] -> [a] -> [a]
-- tal que '(sustituyeLstS xs zs)' es el resultado de aplicar la sustitución
-- 'xs' a los elementos de la lista 'zs'. Por ejemplo,
--   sustituyeLstS sustitucion "2151"     ==  "nada"
--   sustituyeLstS sustitucion "3451"     ==  "vida"
--   sustituyeLstS sustitucion "2134515"  ==  "navidad"
-- ----------------------------------------------------------------------------

sustituyeLstS :: (Eq a) => [(a,a)] -> [a] -> [a]
sustituyeLstS xs zs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 12. Definir usando funciones de orden superior la función
--   segmentosS :: (a -> Bool) -> [a] -> [a]
-- tal que '(segmentosS p xs)' es la lista de los segmentos de la lista 'xs'
-- cuyos elementos verifican la propiedad 'p'. Por ejemplo,
--   segmentosS even [1,2,0,4,5,6,48,7,2]  ==  [[2,0,4],[6,48],[2]]
-- ----------------------------------------------------------------------------
fe :: (a -> Bool) -> [a] -> [[a]]
fe p [x] | p x == True = [[x]]
        | otherwise = [[]]

segmentosS :: (a -> Bool) -> [a] -> [[a]]
segmentosS p [] = []
segmentosS p [x] = fe p [x]
segmentosS p xs = [takeWhile p xs] ++ segmentosS p (tail(dropWhile p xs))

-- ----------------------------------------------------------------------------
-- Ejercicio 13. Definir usando funciones de orden superior la función
--   relacionadosS :: (a -> a -> Bool) -> [a] -> Bool
-- tal que '(relacionadosS r xs)' se verifica si para todo par '(x,y)' de
-- elementos consecutivos de la lista 'xs' se cumple la relación 'r'. Por
-- ejemplo,
--   relacionadosS (<) [2,3,7,9]  ==  True
--   relacionadosS (<) [2,3,1,9]  ==  False
-- ----------------------------------------------------------------------------
prop :: (a -> a -> Bool) -> (a,a) -> Bool
prop r (a,b) = r a b

relacionadosS :: (a -> a -> Bool) -> [a] -> Bool
relacionadosS r xs = and(map (prop r) (zip xs (tail xs)))


-- ----------------------------------------------------------------------------
-- Ejercicio 14. Definir usando funciones de orden superior la función
--   reparteS e xs :: (Ord a) => [a] -> a -> ([a],[a])
-- tal que '(reparteS e xs)' devuelve el par '(ys,zs)', donde 'ys' contiene los
-- elementos de la lista 'xs' estrictamente menores que 'e', mientras que 'zs'
-- contiene los elementos de la lista 'xs' estrictamente mayores que 'e'. Por
-- ejemplo,
--   reparteS 5 [6,7,2,8,6,3,4]  ==  ([2,3,4],[6,7,8,6])
--   reparteS 2 [1,2,3]          ==  ([1],[3])
-- ----------------------------------------------------------------------------

reparteS :: (Ord a) => a -> [a] -> ([a],[a])
reparteS e xs = (filter ( <e) xs, filter ( >=e) xs)

-- ----------------------------------------------------------------------------
-- Ejercicio 14.1. Comprobar con QuickCheck que si '(ys,zs)' es el par obtenido
-- aplicándo la función 'reparteS' al elemento 'e' y la lista 'xs', entonces
-- la suma de las longitudes de las listas 'ys' y 'zs' es menor o igual que la
-- longitud de 'xs'.
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_reparteS1 :: (Ord a) => a -> [a] -> Bool
prop_reparteS1 e xs = undefined

-- La comprobación es
--   > quickCheck prop_reparteS1

-- ----------------------------------------------------------------------------
-- Ejercicio 14.2. Comprobar con QuickCheck que si '(ys,zs)' es el par obtenido
-- aplicándo la función 'reparteS' al elemento 'e' y la lista 'xs', entonces
-- todos los elementos de la lista 'ys' son estrictamente menores que todos los
-- elementos de la lista 'zs'.
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_reparteS2 :: (Ord a) => a -> [a] -> Bool
prop_reparteS2 e xs = undefined

-- La comprobación es
--   > quickCheck prop_reparteS2

-- ----------------------------------------------------------------------------
-- Ejercicio 14.3. Comprobar con QuickCheck que si '(ys,zs)' es el par obtenido
-- aplicándo la función 'reparteS' al elemento 'e' y la lista 'xs', entonces
-- 'e' no pertenece a la lista 'ys' ni a la lista 'zs'.
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_reparteS3 :: (Ord a) => a -> [a] -> Bool
prop_reparteS3 e xs = undefined

-- La comprobación es
--   > quickCheck prop_reparteS3

-- ----------------------------------------------------------------------------
-- Ejercicio 15. Definir usando funciones de orden superior la función
--   agrupaS :: Eq a => [[a]] -> [[a]]
-- tal que '(agrupaS xss)' es la lista de las listas obtenidas agrupando los
-- primeros elementos de los elementos de 'xss', los segundos elementos, ...,
-- hasta la longitud del elemento de 'xss' mas corto. Por ejemplo,
--   agrupaS [[1..6],[7..9],[10..20]]  ==  [[1,7,10],[2,8,11],[3,9,12]]
--   agrupaS []                        ==  []
-- ----------------------------------------------------------------------------

agrupaS :: Eq a => [[a]] -> [[a]]
agrupaS [] = []
agrupaS xss = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 16. Definir usando funciones de orden superior la función
--   superparS :: Int -> Bool
-- tal que '(superparS n)' se verifica si 'n' es un número entero tal que todos
-- sus dígitos son pares. Por ejemplo,
--   superparS 426   ==  True
--   superparS 456   ==  False
--   superparS (-2)  ==  True
-- ----------------------------------------------------------------------------
digitos :: Int -> [Int]
digitos n | elem n [0..9] = [n]
          | otherwise = digitos (n`div`10) ++ [n`mod`10]

superparS :: Int -> Bool
superparS n = all even (digitos n)

-- ============================================================================
-- Definiciones por plegado
-- ============================================================================

-- ----------------------------------------------------------------------------
-- Ejercicio 17. Definir usando 'foldr' la función
--   cuadradosF :: [Int] -> [Int]
-- tal que '(cuadradosF xs)' es la lista de los cuadrados de la lista 'xs'. Por
-- ejemplo,
--   cuadradosF [1,2,3]  ==  [1,4,9]
-- ----------------------------------------------------------------------------

cuadradosF :: [Int] -> [Int]
cuadradosF xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 18. Definir usando 'foldr' la función
--   imparesF :: [Int] -> [Int]
-- tal que '(imparesF xs)' es la lista de los números impares de la lista 'xs'.
-- Por ejemplo,
--   imparesF [1,2,3]  ==  [1,3]
-- ----------------------------------------------------------------------------

imparesF :: [Int] -> [Int]
imparesF xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 19. Definir usando 'foldr' la función
--   sumaCuadradosImparesF :: [Int] -> Int
-- tal que '(sumaCuadradosImparesF xs)' es la suma de los cuadrados de los
-- números impares de la lista 'xs'. Por ejemplo,
--   sumaCuadradosImparesF [1,2,3]  ==  10
-- ----------------------------------------------------------------------------

sumaCuadradosImparesF :: [Int] -> Int
sumaCuadradosImparesF xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 20. Definir usando 'foldr' la función
--   enRangoF :: Int -> Int -> [Int] -> [Int]
-- tal que '(enRangoF a b xs)' es la lista de los elementos de la lista 'xs'
-- que son mayores o iguales que 'a' y menores o iguales que 'b'. Por ejemplo,
--   enRangoF  5 10 [1..15]  ==  [5,6,7,8,9,10]
--   enRangoF 10  5 [1..15]  ==  []
--   enRangoF  5  5 [1..15]  ==  [5]
-- ----------------------------------------------------------------------------

enRangoF :: Int -> Int -> [Int] -> [Int]
enRangoF a b xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 21. Definir usando 'foldr' la función
--   sustituyeImparF :: [Int] -> [Int]
-- tal que '(sustituyeImparF xs)' es la lista obtenida sustituyendo cada número
-- impar de la lista 'xs' por el siguiente número par. Por ejemplo,
--   sustituyeImparF [2,5,7,4]  ==  [2,6,8,4]
-- ----------------------------------------------------------------------------

sustituyeImparF :: [Int] -> [Int]
sustituyeImparF xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 22. Definir usando 'foldr' la función
--   expandeParesF :: [Int] -> [Int]
-- tal que '(expandeParesF xs)' es la lista obtenida a partir de la lista 'xs'
-- repitiendo cada uno de sus elementos pares. Por ejemplo,
--   expandeParesF [3,5,4,6,6,1,0]  ==  [3,5,4,4,6,6,6,6,1,0,0]
--   expandeParesF [3,5,4,6,8,1,0]  ==  [3,5,4,4,6,6,8,8,1,0,0]
-- ----------------------------------------------------------------------------

expandeParesF :: [Int] -> [Int]
expandeParesF xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 23. Una expresión factorizada es una secuencia de pares formados
-- por un número y un exponente. La expansión de una expresión factorizada es
-- el producto de todos los números elevados al exponente que los acompaña. Por
-- ejemplo [(2,2),(3,1),(5,3)] es una expresión factorizada cuya expansión es
-- 2²*3¹*5³ = 1500
--
-- Definir usando 'foldr' la función
--   expansionF :: [(Int,Int)] -> Int
-- tal que '(expansionF xs)' es la expansión de la expresión factorizada 'xs'.
-- Por ejemplo,
--   expansionF [(2,2),(3,1),(5,3)]  ==  1500
-- ----------------------------------------------------------------------------

expansionF :: [(Int,Int)] -> Int
expansionF xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 24. La distancia Hamming entre dos listas es el número de
-- posiciones en las que los correspondientes elementos son distintos. Por
-- ejemplo, la distancia Hamming entre "roma" y "loba" es 2 (porque hay 2
-- posiciones en las que los elementos correspondientes son distintos: la 1ª y
-- la 3ª).
--
-- Definir usando 'foldr' la función
--   distanciaHammingF :: Eq a => [a] -> [a] -> Int
-- tal que '(distanciaHamming xs ys)' es la distancia Hamming entre las listas
-- 'xs' e 'ys'. Por ejemplo,
--   distanciaHammingF "romano" "comino"  ==  2
--   distanciaHammingF "romano" "camino"  ==  3
--   distanciaHammingF "roma"   "comino"  ==  2
--   distanciaHammingF "roma"   "camino"  ==  3
--   distanciaHammingF "romano" "ron"     ==  1
--   distanciaHammingF "romano" "cama"    ==  2
--   distanciaHammingF "romano" "rama"    ==  1
-- ----------------------------------------------------------------------------

distanciaHammingF :: Eq a => [a] -> [a] -> Int
distanciaHammingF xs ys = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 25. El trozo inicial de los elementos de una lista que cumplen
-- una propiedad es la secuencia de elementos de dicha lista desde la posición
-- 0 hasta el primer elemento que no cumple la propiedad, sin incluirlo. Por
-- ejemplo, el trozo inicial de los elementos de [2,4,3,2] que son pares es
-- [2,4].
--
-- Definir usando 'foldr' la función
--   trozoInicialParesF :: [Int] -> [Int]
-- tal que '(trozoInicialParesF xs)' es el trozo inicial de los elementos de
-- la lista 'xs' que son pares. Por ejemplo,
--   trozoInicialParesF []         ==  []
--   trozoInicialParesF [1,2,3,4]  ==  []
--   trozoInicialParesF [2,4,3,2]  ==  [2,4]
--   trozoInicialParesF [2,4,6,8]  ==  [2,4,6,8]
-- ----------------------------------------------------------------------------

trozoInicialParesF :: [Int] -> [Int]
trozoInicialParesF xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 26. Una sustitución es una lista de parejas [(x1,y1),...,(xn,yn)]
-- que se usa para indicar que hay que reemplazar cualquier ocurrencia de cada
-- uno de los xi, por el correspondiente yi. Por ejemplo,
--   [('1','a'),('2','n'),('3','v'),('4','i'),('5','d')]
-- es la sustitución que reemplaza '1' por 'a', '2' por 'n', ...
--
-- Definir usando 'foldr' la función
--   sustituyeEltF :: (Eq a) => [(a,a)] -> a -> a
-- tal que '(sustituyeEltF xs z)' es el resultado de aplicar la sustitución
-- 'xs' al elemento 'z'. Por ejemplo,
--   sustituyeEltF sustitucion '4'  ==  'i'
--   sustituyeEltF sustitucion '2'  ==  'n'
--   sustituyeEltF sustitucion '0'  ==  '0'
-- ----------------------------------------------------------------------------

sustituyeEltF :: (Eq a) => [(a,a)] -> a -> a
sustituyeEltF xs z = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 27. Definir usando 'foldr' la función
--   sustituyeLstF :: (Eq a) => [(a,a)] -> [a] -> [a]
-- tal que '(sustituyeLstF xs zs)' es el resultado de aplicar la sustitución
-- 'xs' a los elementos de la lista 'zs'. Por ejemplo,
--   sustituyeLstF sustitucion "2151"     ==  "nada"
--   sustituyeLstF sustitucion "3451"     ==  "vida"
--   sustituyeLstF sustitucion "2134515"  ==  "navidad"
-- ----------------------------------------------------------------------------

sustituyeLstF :: (Eq a) => [(a,a)] -> [a] -> [a]
sustituyeLstF xs zs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 28. Definir usando 'foldr' la función
--   relacionadosF :: (a -> a -> Bool) -> [a] -> Bool
-- tal que '(relacionadosF r xs)' se verifica si para todo par '(x,y)' de
-- elementos consecutivos de la lista 'xs' se cumple la relación 'r'. Por
-- ejemplo,
--   relacionadosF (<) [2,3,7,9]  ==  True
--   relacionadosF (<) [2,3,1,9]  ==  False
-- ----------------------------------------------------------------------------

relacionadosF :: (a -> a -> Bool) -> [a] -> Bool
relacionadosF r xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 29. Definir usando 'foldr' la función
--   reparteF e xs :: (Ord a) => a -> [a] -> ([a],[a])
-- tal que '(reparteF e xs)' devuelve el par '(ys,zs)', donde 'ys' contiene los
-- elementos de la lista 'xs' estrictamente menores que 'e', mientras que 'zs'
-- contiene los elementos de la lista 'xs' estrictamente mayores que 'e'. Por
-- ejemplo,
--   reparteF 5 [6,7,2,8,6,3,4]  ==  ([2,3,4],[6,7,8,6])
--   reparteF 2 [1,2,3]          ==  ([1],[3])
-- ----------------------------------------------------------------------------

reparteF :: (Ord a) => a -> [a] -> ([a],[a])
reparteF e xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 30. Definir usando 'foldr' la función
--   superparF :: Int -> Bool
-- tal que '(superparF n)' se verifica si 'n' es un número entero tal que todos
-- sus dígitos son pares. Por ejemplo,
--   superparF 426  ==  True
--   superparF 456  ==  False
-- ----------------------------------------------------------------------------

superparF :: Int -> Bool
superparF n = undefined

-- ============================================================================
