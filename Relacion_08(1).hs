-- I1M: Relacion_08.hs
-- Funciones de orden superior y definiciones por plegados (II)
-- Departamento de Ciencias de la Computación e Inteligencia Artificial
-- Universidad de Sevilla
-- ============================================================================

-- ============================================================================
-- Librerías auxiliares
-- ============================================================================

import Data.Char

-- ----------------------------------------------------------------------------
-- Ejercicio 1. Se considera la función
--   filtraAplica :: (a -> b) -> (a -> Bool) -> [a] -> [b]
-- tal que '(filtraAplica f p xs)' es la lista obtenida aplicando la función
-- 'f' a los elementos de la lista 'xs' que cumplen la propiedad 'p'. Por
-- ejemplo,
--   filtraAplica (4+) (<3) [1..7]  ==  [5,6]
--
-- Definir esta función
-- 1) por comprensión,
-- 2) usando funciones de orden superior,
-- 3) por recursión y
-- 4) por plegado (con 'foldr').
-- ----------------------------------------------------------------------------

-- 1) La definición por comprensión es
filtraAplicaC :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplicaC f p xs = [f x | x<-xs, p x]

-- 2) La definición usando funciones de orden superior es
filtraAplicaS :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplicaS f p xs = map f (filter p xs)

-- 3) La definición por recursión es
filtraAplicaR :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplicaR f p [] = []
filtraAplicaR f p (x:xs) | p x = (f x) : filtraAplicaR f p xs
                         |otherwise = filtraAplicaR f p xs

-- 4) La definición por plegado ('foldr') es
filtraAplicaP :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplicaP f p xs = foldr (\ x xs -> if p x then (f x):xs else xs) [] xs

-- ----------------------------------------------------------------------------
-- Ejercicio 2. Se considera la función
--   sumaDigitos :: String -> Int
-- tal que '(sumaDigitos xs)' es la suma de los dígitos de la cadena 'xs'. Por
-- ejemplo,
--   sumaDigitos "SE 2431 X"  ==  10
--
-- Definir esta función
-- 1) por comprensión,
-- 2) usando funciones de orden superior,
-- 3) por recursión y
-- 4) por plegado (con 'foldr').
-- Nota: Usar las funciones '(isDigit c)', que comprueba si el carácter 'c' es
-- un dígito, y '(digitToInt d)', que es el entero correspondiente al dígito
-- 'd'.
-- ----------------------------------------------------------------------------

-- 1) La definición por comprensión es
sumaDigitosC :: String -> Int
sumaDigitosC xs = sum[digitToInt x | x<-xs, isDigit x]

-- 2) La definición usando funciones de orden superior es
sumaDigitosS :: String -> Int
sumaDigitosS xs = foldr (+) 0 (map digitToInt (filter isDigit xs))

-- 3) La definición por recursión es
sumaDigitosR :: String -> Int
sumaDigitosR [] = 0
sumaDigitosR (x:xs) |isDigit x = (digitToInt x)+sumaDigitosR xs
                    |otherwise = sumaDigitosR xs

-- 4) La definición por plegado ('foldr') es
sumaDigitosP :: String -> Int
sumaDigitosP xs = foldr (+) 0 (foldr (\ x xs -> if isDigit x then (digitToInt x):xs else xs) [] xs)

-- ----------------------------------------------------------------------------
-- Ejercicio 3. Se considera la función
--   mayusculaInicial :: String -> String
-- tal que '(mayusculaInicial xs)' es la palabra 'xs' con la letra inicial en
-- mayúscula y las restantes en minúsculas. Por ejemplo,
--   mayusculaInicial "sEviLLa"  ==  "Sevilla"
--
-- Definir esta función
-- 1) por comprensión,
-- 2) usando funciones de orden superior,
-- 3) por recursión y
-- 4) por plegado (con 'foldr').
-- Nota: Usar las funciones '(toLower c)', que devuelve el carácter 'c' en
-- minúscula, y '(toUpper c)', que devuelve el carácter 'c' en mayúscula.
-- ----------------------------------------------------------------------------

--No se como hacerlo por recursion ni por foldr

-- 1) La definición por comprensión es
mayusculaInicialC :: String -> String
mayusculaInicialC xs = toUpper (head xs) : [toLower x | x<-(tail xs)]

-- 2) La definición usando funciones de orden superior es
mayusculaInicialS :: String -> String
mayusculaInicialS xs = toUpper (head xs) : map toLower xs

-- 3) La definición por recursión es
mayusculaInicialR :: String -> String
mayusculaInicialR xs = toUpper (head xs) : [toLower x | x<-(tail xs)]

-- 4) La definición por plegado ('foldr') es
mayusculaInicialP :: String -> String
mayusculaInicialP xs = toUpper (head xs) : map toLower xs

-- ----------------------------------------------------------------------------
-- Ejercicio 4. Se considera la función
--   suma :: Num b => (a -> b) -> [a] -> b
-- tal que '(suma f xs)' es la suma de los valores obtenidos aplicando la
-- función 'f' a los elementos de la lista 'xs'. Por ejemplo,
--   suma (*2)  [3,5,10]  ==  36
--   suma (/10) [3,5,10]  ==  1.8
--
-- Definir esta función
-- 1) por comprensión,
-- 2) usando funciones de orden superior,
-- 3) por recursión y
-- 4) por plegado (con 'foldr').
-- ----------------------------------------------------------------------------

-- 1) La definición por comprensión es
sumaC :: Num b => (a -> b) -> [a] -> b
sumaC f xs = sum[f x | x<-xs]

-- 2) La definición usando funciones de orden superior es
sumaS :: Num b => (a -> b) -> [a] -> b
sumaS f xs = sum(map f xs)

-- 3) La definición por recursión es
sumaR :: Num b => (a -> b) -> [a] -> b
sumaR f [] = 0
sumaR f (x:xs) = f x + sumaR f xs

-- 4) La definición por plegado ('foldr') es
sumaP :: Num b => (a -> b) -> [a] -> b
sumaP f xs = foldr (\ x xs -> f x +xs) 0 xs

-- ----------------------------------------------------------------------------
-- Ejercicio 5. Se considera la función
--   productoPred :: Num a => (a -> Bool) -> [a] -> a
-- tal que '(productoPred p xs)' es el producto de los elementos de la lista
-- 'xs' que cumplen la propiedad 'p'. Por ejemplo,
--   productoPred even [2,1,-3,4,-5,6]  ==  48
--
-- Definir esta función
-- 1) por comprensión,
-- 2) usando funciones de orden superior,
-- 3) por recursión y
-- 4) por plegado (con 'foldr').
-- ----------------------------------------------------------------------------

-- 1) La definición por comprensión es
productoPredC :: Num a => (a -> Bool) -> [a] -> a
productoPredC p xs = undefined

-- 2) La definición usando funciones de orden superior es
productoPredS :: Num a => (a -> Bool) -> [a] -> a
productoPredS p xs = undefined

-- 3) La definición por recursión es
productoPredR :: Num a => (a -> Bool) -> [a] -> a
productoPredR p xs = undefined

-- 4) La definición por plegado ('foldr') es
productoPredP :: Num a => (a -> Bool) -> [a] -> a
productoPredP p xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 6. Se denomina cola de una lista a una sublista no vacía formada
-- por todos los elementos desde una posición hasta el final. Por ejemplo,
-- [3,4,5] es una cola de la lista [1,2,3,4,5].
--
-- Consideramos la función
--   colas :: [a] -> [[a]]
-- tal que '(colas xs)' es la lista de las colas de la lista 'xs'. Por ejemplo,
--   colas []         ==  []
--   colas [1,2]      ==  [[1,2],[2]]
--   colas [4,1,2,5]  ==  [[4,1,2,5],[1,2,5],[2,5],[5]]
--
-- Definir esta función
-- 1) por comprensión,
-- 2) usando funciones de orden superior,
-- 3) por recursión y
-- 4) por plegado (con 'foldr').
-- ----------------------------------------------------------------------------

-- 1) La definición por comprensión es
colasC :: [a] -> [[a]]
colasC xs = [drop i xs | i<-[0..(length xs -1)]]

-- 2) La definición usando funciones de orden superior es
colasS :: [a] -> [[a]]
colasS xs = undefined

-- 3) La definición por recursión es
colasR :: [a] -> [[a]]
colasR xs = [xs]++colasR (tail xs) 

-- 4) La definición por plegado ('foldr') es
colasP :: [a] -> [[a]]
colasP xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 7. Se considera la función
--   posiciones :: String -> Char -> [Int]
-- tal que '(posicionesC xs c)' es la lista de la posiciones del carácter 'c'
-- en la cadena 'xs'. Por ejemplo,
--   posiciones "Salamanca" 'a'  ==  [1,3,5,8]
--
-- Definir esta función
-- 1) por comprensión,
-- 2) usando funciones de orden superior,
-- 3) por recursión y
-- 4) por plegado (con 'foldr').
-- ----------------------------------------------------------------------------

-- 1) La definición por comprensión es
posicionesC :: String -> Char -> [Int]
posicionesC xs c = [i | i<-[0..(length xs -1)], xs!!i == c]

-- 2) La definición usando funciones de orden superior es
posicionesS :: String -> Char -> [Int]
posicionesS xs c = undefined

-- 3) La definición por recursión es
posicionesR :: String -> Char -> [Int]
posicionesR xs c = undefined

-- 4) La definición por plegado ('foldr') es
posicionesP :: String -> Char -> [Int]
posicionesP xs c = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 8. Se considera la función
--   buscaCrucigrama :: Char -> Int -> Int -> [String] -> [String]
-- tal que '(buscaCrucigrama x i l ps)' es la lista de los elementos de la
-- lista de palabras 'ps', que tienen longitud 'l' y tienen la letra 'x' en la
-- posición 'i' (comenzando en 0). Por ejemplo,
--   buscaCrucigrama 'c' 1 7 ["ocaso", "casa", "ocupado"]  ==  ["ocupado"]
--
-- Definir esta función
-- 1) por comprensión,
-- 2) usando funciones de orden superior,
-- 3) por recursión y
-- 4) por plegado (con 'foldr').
-- ----------------------------------------------------------------------------

-- 1) La definición por comprensión es
buscaCrucigramaC :: Char -> Int -> Int -> [String] -> [String]
buscaCrucigramaC x i l ps = undefined

-- 2) La definición usando funciones de orden superior es
buscaCrucigramaS :: Char -> Int -> Int -> [String] -> [String]
buscaCrucigramaS x i l ps = undefined

-- 3) La definición por recursión es
buscaCrucigramaR :: Char -> Int -> Int -> [String] -> [String]
buscaCrucigramaR x i l ps = undefined

-- 4) La definición por plegado ('foldr') es
buscaCrucigramaP :: Char -> Int -> Int -> [String] -> [String]
buscaCrucigramaP x i l ps = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 9. Se considera la función
--   prefijo :: String -> String -> Bool
-- tal que '(prefijo xs ys)' se verifica si la cadena 'xs' es un prefijo de la
-- cadena 'ys'. Por ejemplo,
--   prefijo "casa" "casablanca"  ==  True
--   prefijo "casas" "casa"       ==  False
--   prefijo "casa" "piso"        ==  False
--
-- Definir esta función
-- 1) por comprensión,
-- 2) usando funciones de orden superior,
-- 3) por recursión y
-- 4) por plegado (con 'foldr').
-- ----------------------------------------------------------------------------

-- 1) La definición por comprensión es
prefijoC :: String -> String -> Bool
prefijoC xs ys = undefined

-- 2) La definición usando funciones de orden superior es
prefijoS :: String -> String -> Bool
prefijoS xs ys = undefined

-- 3) La definición por recursión es
prefijoR :: String -> String -> Bool
prefijoR xs ys = undefined

-- 4) La definición por plegado ('foldr') es
prefijoP :: String -> String -> Bool
prefijoP xs ys = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 10. Se considera la función
--   contiene :: String -> String -> Bool
-- tal que '(contiene xs ys)' se verifica si la cadena 'ys' es una subcadena de
-- 'xs', es decir, la cadena 'ys' aparece dentro de la cadena 'xs'. Por
-- ejemplo,
--   contiene "escasa" "casa"    ==  True
--   contiene "escasa" "casada"  ==  False
--   contiene "casa" "escasa"    ==  False
--
-- Definir esta función
-- 1) por comprensión,
-- 2) usando funciones de orden superior,
-- 3) por recursión y
-- 4) por plegado (con 'foldr').
-- ----------------------------------------------------------------------------

-- 1) La definición por comprensión es
contieneC :: String -> String -> Bool
contieneC xs ys = undefined

-- 2) La definición usando funciones de orden superior es
contieneS :: String -> String -> Bool
contieneS xs ys = undefined

-- 3) La definición por recursión es
contieneR :: String -> String -> Bool
contieneR xs ys = undefined

-- 4) La definición por plegado ('foldr') es
contieneP :: String -> String -> Bool
contieneP xs ys = undefined

-- ============================================================================
