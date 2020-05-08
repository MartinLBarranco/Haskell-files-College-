-- I1M: Relación 23
-- El TAD de las pilas
-- Departamento de Ciencias de la Computación e Inteligencia Artificial
-- Universidad de Sevilla
-- ============================================================================

-- ============================================================================
-- Observación
-- ============================================================================

-- Para realizar los ejercicios hay que descargar, en el mismo directorio que
-- el enunciado, el código de los TAD
--   PilaConListas.hs
--   PilaConTipoDeDatoAlgebraico.hs
--
-- El objetivo es hacer los ejercicios con una implementación (por ejemplo con
-- PilaConListas) y comprobar que las definiciones también son válidas con la
-- otra.

--                cima 
--     poner ->  -------  <- quitar
--               -------
--               -------
--               -------      LIFO
--            =============

-- ============================================================================
-- Librerías
-- ============================================================================

import Data.List
import Test.QuickCheck
import I1M.Pila

-- Hay que elegir una implementación del TAD pilas.
--import PilaConListas
-- import PilaConTipoDeDatoAlgebraico

-- · Identificador del TAD
--    Pila
-- · Funciones constructoras
--    vacia :: Pila a
--    apila :: a -> Pila a -> Pila a
-- · Funciones de acceso
--    cima :: Pila a -> a
--    desapila :: Pila a -> Pila a
-- · Funciones identificadoras
--    esVacia :: Pila a -> Bool

-- Propiedades:
--    Si p no está vacía entonces -> apila (cima p) (desapila p) = p
--    cima (apila e p) = e
--    desapila (apila e p) = p

prop_apila_cima_desapila p =
  not (esVacia p) ==> apila (cima p) (desapila p) == p

prop_cima_apila e p =
  cima (apila e p) == e

prop_desapila_apila e p =
  desapila (apila e p) == p

-- ----------------------------------------------------------------------------
-- Ejercicio 1: Definir una función
--   lista2Pila :: [a] -> Pila a
-- tal que '(lista2Pila xs)' es una pila formada por los elementos de la lista
-- 'xs' en el mismo orden, considerando que el primer elemento de la lista es
-- la cima de la pila. Por ejemplo,
--   lista2Pila [1..6]  =>
--     1|2|3|4|5|6#
-- ----------------------------------------------------------------------------

lista2Pila :: [a] -> Pila a
lista2Pila [] = vacia
lista2Pila (x:xs) = apila (x) (lista2Pila xs)

-- ----------------------------------------------------------------------------
-- Ejercicio 2: Definir una función
--   pila2Lista :: Pila a -> [a]
-- tal que '(pila2Lista p)' es la lista formada por los elementos de la pila
-- 'p' en el mismo orden, considerando que el primer elemento de la lista es la
-- cima de la pila. Por ejemplo,
--   pila2Lista (lista2Pila [1..4])  ==  [1,2,3,4]
-- ----------------------------------------------------------------------------

pila2Lista :: Pila a -> [a]
pila2Lista p | esVacia p = []
             | otherwise = cima p : (pila2Lista (desapila p))

-- ----------------------------------------------------------------------------
-- Ejercicio 3: Comprobar con QuickCheck que la función 'pila2Lista' es
-- la inversa de 'lista2Pila', y recíprocamente.
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_pila2Lista :: (Eq a) => Pila a -> Bool
prop_pila2Lista = undefined

-- La comprobación es
--   > quickCheck prop_pila2Lista

-- La propiedad es
prop_lista2Pila :: (Eq a) => [a] -> Bool
prop_lista2Pila = undefined

-- La comprobación es
--   > quickCheck prop_lista2Pila

-- ----------------------------------------------------------------------------
-- Ejercicio 4: Definir la función
--   filtraPila :: (a -> Bool) -> Pila a -> Pila a
-- tal que '(filtraPila q p)' es la pila con los elementos de la pila 'p' que
-- verifican el predicado 'q', en el mismo orden. Por ejemplo,
--   filtraPila even (lista2Pila [1..10])  =>
--     2|4|6|8|10#
-- ----------------------------------------------------------------------------

filtraPila :: (a -> Bool) -> Pila a -> Pila a
filtraPila q p | esVacia p = vacia
               | q (cima p) = apila (cima p) (filtraPila q (desapila p))
               | otherwise = filtraPila q (desapila p)

-- ----------------------------------------------------------------------------
-- Ejercicio 5: Definir la función
--   mapPila :: (a -> a) -> Pila a -> Pila a
-- tal que '(mapPila f p)' es la pila formada con las imágenes por la función
-- 'f' de los elementos de la pila 'p', en el mismo orden. Por ejemplo,
--   mapPila (+7) (lista2Pila [1..5])  =>
--     8|9|10|11|12#
-- ----------------------------------------------------------------------------

mapPila :: (a -> a) -> Pila a -> Pila a
mapPila f p | esVacia p = vacia
            | otherwise = apila (f (cima p)) (mapPila f (desapila p))

-- ----------------------------------------------------------------------------
-- Ejercicio 6: Definir la función
--   pertenecePila :: (Eq a) => a -> Pila a -> Bool
-- tal que '(pertenecePila x p)' se verifica si 'x' es un elemento de la pila
-- 'p'. Por ejemplo,
--   pertenecePila 4 (lista2Pila [1..5])  ==  True
--   pertenecePila 8 (lista2Pila [1..5])  ==  False
-- ----------------------------------------------------------------------------

pertenecePila :: (Eq a) => a -> Pila a -> Bool
pertenecePila x p | esVacia p = False
                  | cima p == x = True
                  | otherwise = pertenecePila x (desapila p)

-- ----------------------------------------------------------------------------
-- Ejercicio 7: definir la función
--   contenidaPila :: (Eq a) => Pila a -> Pila a -> Bool
-- tal que '(contenidaPila p1 p2)' se verifica si todos los elementos de la
-- pila 'p1' son elementos de la pila 'p2'. Por ejemplo,
--   contenidaPila (lista2Pila [3,2,5]) (lista2Pila [1..5])  ==  True
--   contenidaPila (lista2Pila [1..5]) (lista2Pila [3,2,5])  ==  False
-- ----------------------------------------------------------------------------

contenidaPila :: (Eq a) => Pila a -> Pila a -> Bool
contenidaPila q p | esVacia q = True
                  | esVacia p = False
                  | not(pertenecePila (cima q) p) = False
                  | otherwise = contenidaPila (desapila q) p

                    
-- ----------------------------------------------------------------------------
-- Ejercicio 8: Defiir la función
--   prefijoPila :: (Eq a) => Pila a -> Pila a -> Bool
-- tal que '(prefijoPila p1 p2)' se verifica si la pila 'p1' es justamente un
-- prefijo de la pila 'p2'. Por ejemplo,
--   prefijoPila (lista2Pila [1,2,3]) (lista2Pila [1..5])  ==  True
--   prefijoPila (lista2Pila [1..5]) (lista2Pila [1,2,3])  ==  False
-- ----------------------------------------------------------------------------
prefAux :: (Eq a) => [a]->[a]->Bool
prefAux [] _ = True
prefAux _ [] = False
prefAux (y:ys) (x:xs) = x == y && prefAux ys xs


prefijoPila :: (Eq a) => Pila a -> Pila a -> Bool
prefijoPila q p = prefAux (pila2Lista q) (pila2Lista p)

-- ----------------------------------------------------------------------------
-- Ejercicio 9: Definir la función
--   trozoPila :: (Eq a) => Pila a -> Pila a -> Bool
-- tal que '(trozoPila p1 p2)' se verifica si la pila 'p1' es un trozo de la
-- pila 'p2'. Por ejemplo,
--   trozoPila (lista2Pila [2,3,4]) (lista2Pila [1..5])  ==  True
--   trozoPila (lista2Pila [1..5]) (lista2Pila [2,3,4])  ==  False
-- ----------------------------------------------------------------------------

trozoPila :: (Eq a) => Pila a -> Pila a -> Bool
trozoPila q p = prefijoPila q p || trozoPila q (desapila p)

-- ----------------------------------------------------------------------------
-- Ejercicio 10: Defiir la función
--   sufijoPila :: (Eq a) => Pila a -> Pila a -> Bool
-- tal que '(sufijoPila p1 p2)' se verifica si la pila 'p1' es justamente un
-- sufijo de la pila 'p2'. Por ejemplo,
--   sufijoPila (lista2Pila [3,4,5]) (lista2Pila [1..5])  ==  True
--   sufijoPila (lista2Pila [1..5]) (lista2Pila [3,4,5])  ==  False
-- ----------------------------------------------------------------------------

sufijoPila :: (Eq a) => Pila a -> Pila a -> Bool
sufijoPila q p = prefAux (reverse (pila2Lista q)) (reverse (pila2Lista p)) 

-- ----------------------------------------------------------------------------
-- Ejercicio 11: Definir la función
--   ordenadaPila :: (Ord a) => Pila a -> Bool
-- tal que '(ordenadaPila p)' se verifica si los elementos de la pila 'p' están
-- ordenados en orden creciente. Por ejemplo,
--   ordenadaPila (lista2Pila [1..5])   ==  True
--   ordenadaPila (lista2Pila [5,3,4])  ==  False
-- ----------------------------------------------------------------------------

ordenadaPila :: (Ord a) => Pila a -> Bool
ordenadaPila p = pila2Lista p == sort(pila2Lista p)

-- ----------------------------------------------------------------------------
-- Ejercicio 12: Definir la función
--   insertaOrdenadaPila :: (Ord a) => a -> Pila a -> Pila a
-- tal que '(insertaOrdenadaPila x p)' es la pila que se obtiene insertando el
-- elemento 'x' en la pila ordenada 'p', respetando el orden de los elementos.
-- Por ejemplo,
--   insertaOrdenadaPila 3 (lista2Pila [1,2,4])  =>
--     1|2|3|4#
--   insertaOrdenadaPila 2 (lista2Pila [1,2,4])  =>
--     1|2|2|4#
--   insertaOrdenadaPila 6 (lista2Pila [1,2,4])  =>
--     1|2|4|6#
-- ----------------------------------------------------------------------------
insAux :: Ord a => a->[a]->[a]
insAux n [] = [n]
insAux n (x:xs) | n<=x = n:x:xs
                | otherwise = x : (insAux n xs)

                  
insertaOrdenadaPila :: (Ord a) => a -> Pila a -> Pila a
insertaOrdenadaPila n p | ordenadaPila p = lista2Pila(insAux n (pila2Lista p))
                        | otherwise = error("La pila no está ordenada")

-- ----------------------------------------------------------------------------
-- Ejercicio 13: Definir la función
--   ordenaInsercionPila :: (Ord a) => Pila a -> Pila a
-- tal que '(ordenaInsercionPila p)' es una pila con los elementos de la pila
-- 'p', ordenados por inserción. Por ejemplo,
--   ordenaInsercionPila (lista2Pila [2,1,3,4,5,2,3])  =>
--     1|2|2|3|3|4|5#
-- ----------------------------------------------------------------------------

ordenaInsercionPila :: (Ord a) => Pila a -> Pila a
ordenaInsercionPila = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 14: Comprobar con QuickCheck que el resultado de la función
-- 'ordenaInsercionPila' es una pila ordenada correctamente.
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_ordenaInsercionPila :: (Ord a) => Pila a -> Bool
prop_ordenaInsercionPila = undefined

-- La comprobación es
--   > quickCheck prop_ordenaInsercionPila

-- ----------------------------------------------------------------------------
-- Ejercicio 15: Definir la función
--   nubPila :: (Eq a) => Pila a -> Pila a
-- tal que '(nubPila p)' es una pila con los elementos de la pila 'p' sin
-- repeticiones. Por ejemplo,
--   nubPila (lista2Pila [2,1,3,4,5,2,3])  =>
--     1|4|5|2|3#
-- ----------------------------------------------------------------------------

nubPila :: (Eq a) => Pila a -> Pila a
nubPila = lista2Pila . nub . pila2Lista

-- ----------------------------------------------------------------------------
-- Ejercicio 16: Definir la propiedad siguiente: las composición de las
-- funciones 'nub' y 'pila2Lista' coincide con la composición de las funciones
-- 'pila2Lista' y 'nubPila', y comprobarla con quickCheck. En caso de ser
-- falsa, redefinir la función 'nubPila' para que se verifique la propiedad.
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_nubPila :: (Eq a) => Pila a -> Bool
prop_nubPila = undefined

-- La comprobación es
--   > quickCheck prop_nubPila2

-- ----------------------------------------------------------------------------
-- Ejercicio 17: Definir la función
--   maxPila :: (Ord a) => Pila a -> a
-- tal que '(maxPila p)' sea el mayor de los elementos de la pila 'p'. Por
-- ejemplo,
--   maxPila (lista2Pila [2,1,3,4,5,2,3])  ==  5
-- ----------------------------------------------------------------------------

maxPila :: (Ord a) => Pila a -> a
maxPila = maximum . pila2Lista

-- ----------------------------------------------------------------------------
-- Ejercicio 18: Definir la función
--   replicaPila :: Int -> a -> Pila a
-- tal que '(replicaPila n x)' es la pila obtenida insertando 'n' veces el
-- elemento 'x' en la pila vacía. Por ejemplo,
--   replicaPila 3 2  =>
--     2|2|2#
--   replicaPila 6 1  =>
--     1|1|1|1|1|1#
-- ----------------------------------------------------------------------------

replicaPila :: Int -> a -> Pila a
replicaPila 0 x = vacia
replicaPila n x = apila x (replicaPila (n-1) x)
-- ----------------------------------------------------------------------------
-- Ejercicio 19: Definir la función
--   duplicaPila :: Pila a -> Pila a
-- tal que '(duplicaPila p)' es la pila obtenida duplicando cada elemento de la
-- pila 'p'. Por ejemplo,
--   duplicaPila (lista2Pila [1,2,2,3])  =>
--     1|1|2|2|2|2|3|3#
-- ----------------------------------------------------------------------------
duplicaListaAux :: [a]->[a]
duplicaListaAux [] = []
duplicaListaAux (x:xs) = [x,x]++(duplicaListaAux xs)


duplicaPila :: Pila a -> Pila a
duplicaPila p = lista2Pila ( duplicaListaAux (pila2Lista p))

-- ----------------------------------------------------------------------------
-- Ejercicio 20: Definir la función
--   reducePila :: (Eq a) => Pila a -> Pila a
-- tal que '(reducePila p)' es la pila obtenida eliminando los elementos
-- consecutivos repetidos de la pila 'p'. Por ejemplo,
--   reducePila (lista2Pila [1,1,2,2,2,3,4,4,2,2,3,3])  =>
--     1|2|3|4|2|3#
-- ----------------------------------------------------------------------------
procesa :: Eq a => [a]->[a]
procesa [] = []
procesa [x] = [x]
procesa (x:y:xs) | x==y = procesa (x:xs)
                 | otherwise = x: procesa (y:xs)


reducePila :: (Eq a) => Pila a -> Pila a
reducePila p = lista2Pila (procesa (pila2Lista p))

-- ============================================================================
-- Generador de pilas para QuickCheck
-- ============================================================================

-- genPila es un generador de pilas. Por ejemplo,
--   ...> sample genPila
--   #
--   0|1#
--   #
--   -4|3|-1#
--   0|-4|8|-5#
--   -4#
--   -3|-3|-6|-12|-6|-4|-1|-10|-2|5|-3|6#
--   #
--   13|15|1|-14|4|-6|12|3|16|13|11#
--   11|15|-9|10#
--   12|-12|16|3|-8#

genPila :: (Arbitrary a, Num a) => Gen (Pila a)
genPila = do
  xs <- listOf arbitrary
  return (foldr apila vacia xs)

-- El tipo pila es una instancia del arbitrario.

instance (Arbitrary a, Num a) => Arbitrary (Pila a) where
  arbitrary = genPila

-- ============================================================================
