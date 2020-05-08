-- I1M: Relación 24
-- El TAD de las colas
-- Departamento de Ciencias de la Computación e Inteligencia Artificial
-- Universidad de Sevilla
-- ============================================================================

-- ============================================================================
-- Observación
-- ============================================================================

-- Para realizar los ejercicios hay que descargar, en el mismo directorio que
-- el enunciado, el código de los TAD
--   ColaConListas.hs
--   ColaConDosListas.hs
--   ColaConFunciones.hs
--
-- El objetivo es hacer los ejercicios con una implementación (por ejemplo con
-- ColaConListas) y comprobar que las definiciones también son válidas con la
-- otra.

-- ============================================================================
-- Librerías
-- ============================================================================

import Data.List
import Test.QuickCheck

-- Hay que elegir una implementación del TAD colas.
import I1M.Cola
-- import ColaConDosListas
-- import ColaConFunciones

-- ----------------------------------------------------------------------------
-- Ejercicio 1: Definir una función
--   lista2Cola :: [a] -> Cola a
-- tal que '(lista2Cola xs)' es una cola formada por los elementos de la lista
-- 'xs' en el mismo orden, considerando que el primer elemento de la lista es
-- el primer elemento de la cola. Por ejemplo,
--   lista2Cola [1..6]  =>
--     < 1 2 3 4 5 6 <
-- ----------------------------------------------------------------------------

lista2Cola :: [a] -> Cola a
lista2Cola [] = vacia
lista2Cola xs = inserta  (last xs) (lista2Cola (init xs))

-- ----------------------------------------------------------------------------
-- Ejercicio 2: Definir una función
--   cola2Lista :: Cola a -> [a]
-- tal que '(cola2Lista c)' es la lista formada por los elementos de la cola
-- 'c' en el mismo orden, considerando que el primer elemento de la lista es el
-- primer elemento de la cola. Por ejemplo,
--   cola2Lista (lista2Cola [1..4])  ==  [1,2,3,4]
-- ----------------------------------------------------------------------------

cola2Lista :: Cola a -> [a]
cola2Lista c | esVacia c = []
             | otherwise =(primero c) :(cola2Lista (resto c))

-- ----------------------------------------------------------------------------
-- Ejercicio 3. Comprobar con QuickCheck que la función 'cola2Lista' es
-- la inversa de 'lista2Cola', y recíprocamente.
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_cola2Lista :: (Eq a) => Cola a -> Bool
prop_cola2Lista = undefined

-- La comprobación es
--   > quickCheck prop_cola2Lista

-- La propiedad es
prop_lista2Cola :: (Eq a) => [a] -> Bool
prop_lista2Cola = undefined

-- La comprobación es
--   > quickCheck prop_lista2Cola

-- ----------------------------------------------------------------------------
-- Ejercicio 4: Definir la función
--   ultimoCola :: Cola a -> a
-- tal que '(ultimoCola c)' es el último elemento de la cola 'c'. Por ejemplo,
--   ultimoCola (lista2Cola [1..4])  ==  4
-- ----------------------------------------------------------------------------

ultimoCola :: Cola a -> a
ultimoCola c = last (cola2Lista c)

-- ----------------------------------------------------------------------------
-- Ejercicio 5: Definir la función
--   longitudCola :: Cola a -> Int
-- tal que '(longitudCola c)' es el número de elementos de la cola 'c'. Por
-- ejemplo,
--   longitudCola (lista2Cola [1..6])  ==  6
-- ----------------------------------------------------------------------------

longitudCola :: Cola a -> Int
longitudCola c | esVacia c = 0
               | otherwise =  1 + longitudCola (resto c)

-- ----------------------------------------------------------------------------
-- Ejercicio 6: Definir la función
--   todosCumplen :: (a -> Bool) -> Cola a -> Bool
-- tal que '(todosCumplen p c)' se verifica si todos los elementos de la
-- cola 'c' cumplen la propiedad 'p'. Por ejemplo,
--   todosCumplen (even) (lista2Cola [2,4..10])  ==  True
--   todosCumplen (even) (lista2Cola [1..10])    ==  False
-- ----------------------------------------------------------------------------

todosCumplen :: (a -> Bool) -> Cola a -> Bool
todosCumplen p c |esVacia c = True
                 |otherwise = p (primero c) && todosCumplen p (resto c)

-- ----------------------------------------------------------------------------
-- Ejercicio 7: Definir la función
--   algunoCumple :: (a -> Bool) -> Cola a -> Bool
-- tal que '(algunoCumple p c)' se verifica si algún elemento de la cola
-- 'c' cumple la propiedad 'p'. Por ejemplo,
--   algunoCumple (odd) (lista2Cola [1..10])    ==  True
--   algunoCumple (odd) (lista2Cola [2,4..10])  ==  False
-- ----------------------------------------------------------------------------

algunoCumple :: (a -> Bool) -> Cola a -> Bool
algunoCumple p c |esVacia c = True
                 |otherwise = p (primero c) || todosCumplen p (resto c)

-- ----------------------------------------------------------------------------
-- Ejercicio 8: Definir la función
--   soloUnoCumple :: (a -> Bool) -> Cola a -> Bool
-- tal que '(soloUnoCumple p c)' se verifica si hay un único elemento en la
-- cola 'c' que cumple la propiedad 'p'. Por ejemplo,
--   soloUnoCumple (even) (lista2Cola [1,2,3])  ==  True
--   soloUnoCumple (even) (lista2Cola [1,3,5])  ==  False
--   soloUnoCumple (odd) (lista2Cola [1,2,3])   ==  False
-- ----------------------------------------------------------------------------
atLeastOne :: (a->Bool)->[a]->Bool
atLeastOne p xs | length (filter p xs) == 1 = True
                | otherwise = False


soloUnoCumple :: (a -> Bool) -> Cola a -> Bool
soloUnoCumple p c =atLeastOne p (cola2Lista c) 

-- ----------------------------------------------------------------------------
-- Ejercicio 9: Definir la función
--   desencolaMientras :: (a -> Bool) -> Cola a -> Cola a
-- tal que '(desencolaMientras p c)' es el resultado de sacar elementos de la
-- cola 'c' mientras que cumplan la propiedad 'p', hasta llegar al primero que
-- no la cumpla o que la cola 'c' se quede vacía. Por ejemplo,
--   desencolaMientras (odd) (lista2Cola [1,3,5,2,4,6])   =>
--     < 2 4 6 <
--   desencolaMientras (> 10) (lista2Cola [1,3,5,2,4,6])  =>
--     < 1 3 5 2 4 6 <
--   desencolaMientras (< 10) (lista2Cola [1,3,5,2,4,6])  =>
--     < <
-- ----------------------------------------------------------------------------

desencolaMientras :: (a -> Bool) -> Cola a -> Cola a
desencolaMientras p c =lista2Cola (reverse(takeWhile p (reverse(cola2Lista c))))

-- ----------------------------------------------------------------------------
-- Ejercicio 10: Definir la función
--   seleccionaMientras :: (a -> Bool) -> Cola a -> Cola a
-- tal que '(seleccionaMientras p c)' es el resultado de crear una nueva cola
-- con los primeros elementos de la cola 'c' mientras que cumplan la propiedad
-- 'p', hasta llegar al primero que no la cumpla o que la cola 'c' se quede
-- vacía. Por ejemplo,
--   seleccionaMientras (odd) (lista2Cola [1,3,5,2,4,6])   =>
--     < 1 3 5 <
--   seleccionaMientras (> 10) (lista2Cola [1,3,5,2,4,6])  =>
--     < <
--   seleccionaMientras (< 10) (lista2Cola [1,3,5,2,4,6])  =>
--     < 1 3 5 2 4 6 <
-- ----------------------------------------------------------------------------

seleccionaMientras :: (a -> Bool) -> Cola a -> Cola a
seleccionaMientras = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 11: Definir la función
--   ponAlaCola :: Cola a -> Cola a -> Cola a
-- tal que '(ponAlaCola c1 c2)' es la cola que resulta de poner los elementos
-- de la cola 'c2', en el mismo orden, al final de la cola 'c1'. Por ejemplo,
--   ponAlaCola (lista2Cola [1,3,5]) (lista2Cola [2,4,6])  =>
--     < 1 3 5 2 4 6 <
-- ----------------------------------------------------------------------------

ponAlaCola :: Cola a -> Cola a -> Cola a
ponAlaCola c1 c2 = lista2Cola( (cola2Lista c1)++(cola2Lista c2))

-- ----------------------------------------------------------------------------
-- Ejercicio 12: Comprobar con QuickCheck que dada una cola 'c', si 'c1' es la
-- cola obtenida seleccionando los elementos de 'c' mientras que sean números
-- pares y 'c2' es la cola obtenida sacando los elementos de 'c' mientras que
-- sean números pares, entonces la cola 'c' es igual al resultado de poner 'c2'
-- a la cola de 'c1'.
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_ponAlaColaSeleccionaDesencola :: Cola Int -> Bool
prop_ponAlaColaSeleccionaDesencola = undefined

-- La comprobación es
--   > quickCheck prop_ponAlaColaSeleccionaDesencola

-- ----------------------------------------------------------------------------
-- Ejercicio 13: Definir la función
--   mezclaColas :: Cola a -> Cola a -> Cola a
-- tal que '(mezclaColas c1 c2)' es la cola formada por los elementos de las
-- colas 'c1' y 'c2' colocados en una cola, de forma alternativa, empezando por
-- los elementos de la cola 'c1'. Por ejemplo,
--   mezclaColas (lista2Cola [1,3,5]) (lista2Cola [2,4,6])  =>
--     < 1 2 3 4 5 6 <
-- ----------------------------------------------------------------------------
entreMezcla :: [a] -> [a] -> [a]
entreMezcla [] ys = ys
entreMezcla xs [] = xs
entreMezcla (x:xs) (y:ys) = x : entreMezcla (y:ys) xs  

mezclaColas :: Cola a -> Cola a -> Cola a
mezclaColas c1 c2 = lista2Cola (entreMezcla (cola2Lista c1) (cola2Lista c2))

-- ----------------------------------------------------------------------------
-- Ejercicio 14: Definir la función
--   agrupaColas :: [Cola a] -> Cola a
-- tal que '(agrupaColas [c1,c2,c3,...,cn])' es la cola formada mezclando las
-- colas de la lista como sigue: mezcla 'c1' con 'c2', el resultado con 'c3',
-- el resultado con 'c4', y así sucesivamente. Por ejemplo,
--   agrupaColas (replicate 3 (lista2Cola [1,3,5]))  =>
--     < 1 1 1 3 3 5 3 5 5 <
-- ----------------------------------------------------------------------------

agrupaColas :: [Cola a] -> Cola a
agrupaColas [] = vacia
agrupaColas [c] = c
agrupaColas (x:y:xs) = agrupaColas ((mezclaColas x y):xs)

-- ----------------------------------------------------------------------------
-- Ejercicio 15: Definir la función
--   separaCola :: Cola a -> (Cola a,Cola a)
-- tal que '(separaCola c)' es un par formado por dos colas resultado de
-- repartir los elementos de la cola 'c' de forma alternativa, respetando el
-- orden. Por ejemplo,
--   separaCola (lista2Cola [1,2,3,4,5])    =>
--     (< 1 3 5 <,< 2 4 <)
--   separaCola (lista2Cola [1,2,3,4,5,6])  =>
--     (< 1 3 5 <,< 2 4 6 <)
-- ----------------------------------------------------------------------------

separaCola :: Cola a -> (Cola a,Cola a)
separaCola c = undefined 

-- ----------------------------------------------------------------------------
-- Ejercicio 16: Comprobar con quickCheck que el resultado de mezclar las colas
-- que se obtienen al separar una dada es igual a dicha cola, pero que el
-- resultado de separar el resultado de mezclar dos colas no siempre es igual
-- al par de colas originales.
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_mezclarSeparar :: (Eq a) => Cola a -> Bool
prop_mezclarSeparar = undefined

-- La comprobación es
--   > quickCheck prop_mezclarSeparar

-- La propiedad es
prop_separarMezclar :: (Eq a) => Cola a -> Cola a -> Bool
prop_separarMezclar = undefined

-- La comprobación es
--   > quickCheck prop_separarMezclar

-- ----------------------------------------------------------------------------
-- Ejercicio 17: Definir la función
--   reparteCola :: (a -> Bool) -> Cola a -> (Cola a,Cola a)
-- tal que '(reparteCola p c)' s un par formado por dos colas resultado de
-- repartir los elementos de la cola 'c' en los elementos que cumplen la
-- propiedad 'p' y los que no la cumplen, respetando el orden. Por ejemplo,
--   reparteCola (even) (lista2Cola [1..10])  =>
--     (< 2 4 6 8 10 <,< 1 3 5 7 9 <)
-- ----------------------------------------------------------------------------

reparteCola :: (a -> Bool) -> Cola a -> (Cola a,Cola a)
reparteCola p c = undefined
  
-- ----------------------------------------------------------------------------
-- Ejercicio 18: Definir la función
--   elemCola :: Int -> Cola a -> Maybe a
-- tal que '(elemCola k c)' es el 'k'-ésimo elemento de la cola 'c', contando
-- desde 1 desde el principio, en caso de que exista y Nothing, en caso
-- contrario. Por ejemplo,
--   elemCola 1 (lista2Cola [1,2,3,4,5])  ==  Just 1
--   elemCola 3 (lista2Cola [1,2,3,4,5])  ==  Just 3
--   elemCola 6 (lista2Cola [1,2,3,4,5])  ==  Nothing
-- ----------------------------------------------------------------------------

elemCola :: Int -> Cola a -> Maybe a
elemCola = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 19: Definir la función
--   takeCola :: Int -> Cola a -> Cola a
-- tal que '(takeCola k c)' es la cola formada por los 'k' primeros elementos
-- de la cola 'c', en el mismo orden. Si no hay elementos suficientes en la
-- cola 'c', se tomarán tantos como sea posible. Por ejemplo,
--   takeCola 3 (lista2Cola [1..6])  =>
--     < 1 2 3 <
--   takeCola 5 (lista2Cola [1..3])  =>
--     < 1 2 3 <
-- ----------------------------------------------------------------------------

takeCola :: Int -> Cola a -> Cola a
takeCola = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 20: Definir la función
--   dropCola :: Int -> Cola a -> Cola a
-- tal que '(dropCola k c)' es la cola resultado de quitar los 'k' primeros
-- elementos de la cola 'c'. Si no hay elementos suficientes en la cola 'c', se
-- quitarán tantos como sea posible. Por ejemplo,
--   dropCola 3 (lista2Cola [1..6])  =>
--     < 4 5 6 <
--   dropCola 5 (lista2Cola [1..3])  =>
--     < <
-- ----------------------------------------------------------------------------

dropCola :: Int -> Cola a -> Cola a
dropCola = undefined

-- ============================================================================
-- Generador de colas para QuickCheck
-- ============================================================================

-- genCola es un generador de colas de enteros. Por ejemplo,
--   ...> sample genCola
--   < 0 0 0 0 0 0 0 0 0 0 <
--   < <
--   < -1 -4 -3 -2 -2 -3 <
--   < 5 1 2 3 5 -3 4 -2 1 -5 3 <
--   < 3 2 <
--   < -8 0 -2 8 -5 -4 <
--   < 7 4 2 <
--   < 12 9 -3 11 -6 5 -6 12 11 13 12 -9 8 2 13 10 -11 <
--   < -12 14 -14 -1 -11 1 -6 -1 -15 <
--   < -5 -13 11 -2 -6 18 -3 17 17 -11 -16 6 1 2 -14 10 -16 6 -11 <
--   < -5 4 6 -7 11 0 -1 -8 <

genCola :: (Num a, Arbitrary a) => Gen (Cola a)
genCola =
  frequency [(1, return vacia),
             (20, do n <- choose (1,20)
                     xs <- vectorOf n arbitrary
                     return (creaCola xs))]
  where creaCola = foldr inserta vacia

-- El tipo cola es una instancia del arbitrario.

instance (Arbitrary a, Num a) => Arbitrary (Cola a) where
  arbitrary = genCola

-- ============================================================================
