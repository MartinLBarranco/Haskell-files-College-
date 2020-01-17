-- I1M: Relación 11
-- Tipos de datos algebraicos (I)
-- Departamento de Ciencias de la Computación e Inteligencia Artificial
-- Universidad de Sevilla
-- ============================================================================

-- ============================================================================
-- Librerías auxiliares
-- ============================================================================

import Test.QuickCheck

-- ============================================================================
-- Números complejos
-- ============================================================================

-- ----------------------------------------------------------------------------
-- Los números complejos se pueden representar mediante pares de números
-- reales. Por ejemplo, el número 2+5i se puede representar mediante el par
-- (2,5). En los siguientes ejercicios utilizaremos el tipo de dato algebraico
-- 'Complejo' para representar los números complejos.
-- ----------------------------------------------------------------------------

type Complejo = (Double,Double)

-- ----------------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--   conjugado :: Complejo -> Complejo
-- tal que '(conjugado x)' es el conjugado del número complejo 'x'. Por
-- ejemplo,
--   conjugado (2,3)  ==  (2.0,-3.0)
-- ----------------------------------------------------------------------------

conjugado :: Complejo -> Complejo
conjugado = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--   sumaComplejos :: Complejo -> Complejo -> Complejo
-- tal que '(sumaComplejos x y)' es la suma de los números complejos 'x' e 'y'.
-- Por ejemplo,
--   sumaComplejos (2,3) (5,6)  ==  (7.0,9.0)
-- ----------------------------------------------------------------------------

sumaComplejos :: Complejo -> Complejo -> Complejo
sumaComplejos = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--   productoComplejos :: Complejo -> Complejo -> Complejo
-- tal que '(productoComplejos x y)' es el producto de los números complejos
-- 'x' e 'y'. Por ejemplo,
--   productoComplejos (2,3) (5,6)  ==  (-8.0,27.0)
-- ----------------------------------------------------------------------------

productoComplejos :: Complejo -> Complejo -> Complejo
productoComplejos = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--   cocienteComplejos :: Complejo -> Complejo -> Complejo
-- tal que '(cocienteComplejos x y)' es el cociente de los números complejos
-- 'x' e 'y'. Por ejemplo,
--   cocienteComplejos (3,2) (1,-2)  ==  (-0.2,1.6)
-- ----------------------------------------------------------------------------

cocienteComplejos :: Complejo -> Complejo -> Complejo
cocienteComplejos = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--   potenciaComplejos :: Complejo -> Int -> Complejo
-- tal que '(potenciaComplejos x n)' es la potencia del número complejo 'x'
-- elevado al número natural 'n'. Por ejemplo,
--   potenciaComplejos (3,2) 0  ==  (1.0,0.0)
--   potenciaComplejos (3,2) 1  ==  (3.0,2.0)
--   potenciaComplejos (3,2) 2  ==  (5.0,12.0)
--   potenciaComplejos (3,2) 3  ==  (-9.0,46.0)
-- ----------------------------------------------------------------------------

potenciaComplejos :: Complejo -> Int -> Complejo
potenciaComplejos = undefined

-- ============================================================================
-- Números naturales
-- ============================================================================

-- ----------------------------------------------------------------------------
-- En los siguientes ejercicios se usará el tipo algebraico de datos de los
-- números naturales definido por
--   data Nat = Cero
--            | Suc Nat
--              deriving (Eq, Show)
-- ----------------------------------------------------------------------------

data Nat = Cero
         | Suc Nat
           deriving (Eq, Show)

-- ----------------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--   producto :: Nat -> Nat -> Nat
-- tal que '(producto n m)' es el producto de los números naturales 'n' y 'm'.
-- Por ejemplo,
--   producto (Suc (Suc (Suc (Cero)))) Cero            ==  Cero
--   producto (Suc (Suc Cero)) (Suc (Suc (Suc Cero)))  ==
--     Suc (Suc (Suc (Suc (Suc (Suc Cero)))))
-- ----------------------------------------------------------------------------

producto :: Nat -> Nat -> Nat
producto = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--   potencia :: Nat -> Nat -> Nat
-- tal que '(potencia n m)' es la potencia del número natural 'n' elevado al
-- número natural 'm'. Por ejemplo,
--   potencia (Suc (Suc (Suc (Cero)))) Cero            ==  Suc Cero
--   potencia (Suc (Suc Cero)) (Suc (Suc (Suc Cero)))  ==
--     Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc Cero)))))))
-- ----------------------------------------------------------------------------

potencia :: Nat -> Nat -> Nat
potencia = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 8. Definir la función
--   menor :: Nat -> Nat -> Bool
-- tal que '(menor n m)' se verifica si el número natural 'n' es estrictamente
-- menor que el número natural 'm'. Por ejemplo,
--   menor (Suc (Suc (Suc (Cero)))) Cero            ==  False
--   menor (Suc (Suc Cero)) (Suc (Suc (Suc Cero)))  ==  True
-- ----------------------------------------------------------------------------

menor :: Nat -> Nat -> Bool
menor = undefined

-- ============================================================================
-- Árboles binarios
-- ============================================================================

-- ----------------------------------------------------------------------------
-- En los siguientes ejercicios se usará el tipo algebraico de datos de los
-- árboles binarios definido por
--   data Arbol t = H t
--                | N t (Arbol t) (Arbol t)
--                  deriving (Show, Eq)
--
-- Por ejemplo, el árbol
--         9
--        / \
--       /   \
--      3     7
--     / \
--    2   4
-- se representa por
--   N 9 (N 3 (H 2) (H 4)) (H 7)
-- ----------------------------------------------------------------------------

data Arbol t = H t
             | N t (Arbol t) (Arbol t)
               deriving (Show, Eq)

-- ----------------------------------------------------------------------------
-- Ejercicio 9. Definir la función
--   nHojas :: Arbol t -> Int
-- tal que '(nHojas a)' es el número de hojas del árbol binario 'a'. Por
-- ejemplo,
--   nHojas (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  3
-- ----------------------------------------------------------------------------

nHojas :: Arbol t -> Int
nHojas = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 10. Definir la función
--   nNodos :: Arbol t -> Int
-- tal que '(nNodos a)' es el número de nodos internos del árbol binario 'a'.
-- Por ejemplo,
--   nNodos (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  2
-- ----------------------------------------------------------------------------

nNodos :: Arbol t -> Int
nNodos = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 11. Comprobar con QuickCheck que en todo árbol binario el número
-- de hojas es igual al número de nodos internos más uno.
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_nHojas :: Arbol Int -> Bool
prop_nHojas = undefined

-- La comprobación es
--   > quickCheck prop_nHojas

-- ----------------------------------------------------------------------------
-- Ejercicio 12. La profundidad de un árbol es la longitud de la secuencia más
-- larga de nodos que va desde la raíz hasta un nodo hoja pasando siempre de
-- padres a hijos.
--
-- Definir la función
--   profundidad :: Arbol t -> Int
-- tal que '(profundidad a)' es la profundidad del árbol binario 'a'. Por
-- ejemplo,
--   profundidad (N 9 (N 3 (H 2) (H 4)) (H 7))              ==  2
--   profundidad (N 9 (N 3 (H 2) (N 1 (H 4) (H 5))) (H 7))  ==  3
-- ----------------------------------------------------------------------------

profundidad :: Arbol t -> Int
profundidad = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 13. Comprobar con QuickCheck que para todo árbol binario 'a', se
-- tiene que
--   nNodos a <= 2^(profundidad a) - 1
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_nNodosProfundidad :: Arbol Int -> Bool
prop_nNodosProfundidad = undefined

-- La comprobación es
--   > quickCheck prop_nNodosProfundidad

-- ----------------------------------------------------------------------------
-- Ejercicio 14. Definir la función
--   preorden :: Arbol t -> [t]
-- tal que '(preorden a)' es la lista correspondiente al recorrido en preorden
-- del árbol binario 'a'; es decir, primero se visita la raíz del árbol, a
-- continuación se recorre el subárbol izquierdo y, finalmente, se recorre el
-- subárbol derecho. Por ejemplo,
--   preorden (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  [9,3,2,4,7]
-- ----------------------------------------------------------------------------

preorden :: Arbol t -> [t]
preorden = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 15. Comprobar con QuickCheck que la longitud de la lista obtenida
-- recorriendo un árbol binario en preorden es igual al número de hojas del
-- árbol más el número de nodos internos.
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_longitudPreorden :: Arbol Int -> Bool
prop_longitudPreorden = undefined

-- La comprobación es
--   > quickCheck prop_longitudPreorden

-- ----------------------------------------------------------------------------
-- Ejercicio 16. Definir la función
--   postorden :: Arbol t -> [t]
-- tal que '(postorden a)' es la lista correspondiente al recorrido en
-- postorden del árbol binario 'a'; es decir, primero se recorre el subárbol
-- izquierdo, a continuación se recorre el subárbol derecho y, finalmente, se
-- visita la raíz del árbol. Por ejemplo,
--   postorden (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  [2,4,3,7,9]
-- ----------------------------------------------------------------------------

postorden :: Arbol t -> [t]
postorden = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 17. Definir, por recursión con acumulador, la función
--   preordenAR :: Arbol t -> [t]
-- tal que '(preordenAR a)' es la lista correspondiente al recorrido en
-- preorden del árbol binario 'a'; es decir, primero se visita la raíz del
-- árbol, a continuación se recorre el subárbol izquierdo y, finalmente, se
-- recorre el subárbol derecho. Por ejemplo,
--   preordenAR (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  [9,3,2,4,7]
--
-- Nota: No usar (++) en la definición
-- ----------------------------------------------------------------------------

preordenAR :: Arbol t -> [t]
preordenAR = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 18. Comprobar con QuickCheck que las funciones 'preorden' y
-- 'preordenAR' son equivalentes.
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_preordenAR :: Arbol Int -> Bool
prop_preordenAR = undefined

-- La comprobación es
--   > quickCheck prop_preordenAR

-- ----------------------------------------------------------------------------
-- Ejercicio 19. Definir la función
--   espejo :: Arbol t -> Arbol t
-- tal que '(espejo a)' es la imagen especular del árbol binario 'a'; es decir
-- todos los hijos izquierdos pasan a ser hijos derechos y viceversa. Por
-- ejemplo,
--   espejo (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  N 9 (H 7) (N 3 (H 4) (H 2))
-- ----------------------------------------------------------------------------

espejo :: Arbol t -> Arbol t
espejo = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 20. Comprobar con QuickCheck que para todo árbol binario 'a', se
-- cumple
--   espejo (espejo a) = a
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_espejoEspejo :: Arbol Int -> Bool
prop_espejoEspejo = undefined

-- La comprobación es
--   > quickCheck prop_espejoEspejo

-- ----------------------------------------------------------------------------
-- Ejercicio 21. Comprobar con QuickCheck que para todo árbol binario 'a', se
-- cumple
--   reverse (preorden (espejo a)) = postorden a
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_inversaPreordenEspejo :: Arbol Int -> Bool
prop_inversaPreordenEspejo = undefined

-- La comprobación es
--   > quickCheck prop_inversaPreordenEspejo

-- ----------------------------------------------------------------------------
-- Ejercicio 22. Comprobar con QuickCheck que para todo árbol binario 'a', se
-- cumple
--   postorden (espejo a) = reverse (preorden a)
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_recorrido :: Arbol Int -> Bool
prop_recorrido = undefined

-- La comprobación es
--   > quickCheck prop_recorrido

-- ----------------------------------------------------------------------------
-- Ejercicio 23. Definir la función
--   takeArbol ::  Int -> Arbol t -> Arbol t
-- tal que '(takeArbol n a)' es el subárbol del árbol binario 'a', que contiene
-- todos sus nodos desde la raíz hasta los nodos de profundidad 'n'. Por
-- ejemplo,
--   takeArbol 0 (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  H 9
--   takeArbol 1 (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  N 9 (H 3) (H 7)
--   takeArbol 2 (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  N 9 (N 3 (H 2) (H 4)) (H 7)
--   takeArbol 3 (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  N 9 (N 3 (H 2) (H 4)) (H 7)
-- ----------------------------------------------------------------------------

takeArbol :: Int -> Arbol t -> Arbol t
takeArbol = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 24. Comprobar con QuickCheck que para todo número natural 'n' y
-- para todo árbol binario 'a', la profundidad de '(takeArbol n a)' es menor o
-- igual que 'n'.
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_takeArbol :: Int -> Arbol Int -> Property
prop_takeArbol = undefined

-- La comprobación es
--   > quickCheck prop_takeArbol

-- ----------------------------------------------------------------------------
-- Ejercicio 25. Definir la función
--   repeatArbol :: t -> Arbol t
-- tal que '(repeatArbol x)' es el árbol binario completo, es decir todos los
-- nodos tienen hijos izquierdo y derecho, con infinitos nodos con el valor
-- 'x'. Por ejemplo,
--   takeArbol 0 (repeatArbol 3)  ==  H 3
--   takeArbol 1 (repeatArbol 3)  ==  N 3 (H 3) (H 3)
--   takeArbol 2 (repeatArbol 3)  ==  N 3 (N 3 (H 3) (H 3)) (N 3 (H 3) (H 3))
-- ----------------------------------------------------------------------------

repeatArbol :: t -> Arbol t
repeatArbol = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 26. Definir la función
--   replicateArbol :: Int -> t -> Arbol t
-- tal que '(replicateArbol n x)' es el árbol binario completo de profundidad
-- 'n', es decir todos los nodos internos tienen hijos izquierdo y derecho y
-- todas las hojas están a profundidad 'n'; en el que todos los nodos tienen el
-- valor 'x'. Por ejemplo,
--   replicateArbol 0 5  ==  H 5
--   replicateArbol 1 5  ==  N 5 (H 5) (H 5)
--   replicateArbol 2 5  ==  N 5 (N 5 (H 5) (H 5)) (N 5 (H 5) (H 5))
-- ----------------------------------------------------------------------------

replicateArbol :: Int -> t -> Arbol t
replicateArbol = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 27. Comprobar con QuickCheck que para todo número natural 'n', el
-- número de hojas de '(replicateArbol n x)' es 2^n.
--
-- Nota. Al hacer la comprobación limitar el tamaño de las pruebas como se
-- indica a continuación
--   quickCheckWith (stdArgs {maxSize=7}) prop_replicateArbol
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_replicateArbol :: Int -> Int -> Property
prop_replicateArbol = undefined

-- La comprobación es
--   > quickCheckWith (stdArgs {maxSize=7}) prop_replicateArbol

-- ----------------------------------------------------------------------------
-- Ejercicio 28.1. Definir la función
--   mapArbol :: (t -> t) -> Arbol t -> Arbol t
-- tal que '(mapArbol f a)' es el árbol binario obtenido aplicando la función
-- 'f' a los valores de cada nodo del árbol binario 'a'. Por ejemplo,
--   mapArbol (*2) (N 9 (N 3 (H 2) (H 4)) (H 7))  ==
--     N 18 (N 6 (H 4) (H 8)) (H 14)
-- ----------------------------------------------------------------------------

mapArbol :: (t -> t) -> Arbol t -> Arbol t
mapArbol = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 28.2. Comprobar con QuickCheck que
--   (mapArbol (1+)) . espejo = espejo . (mapArbol (1+))
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_mapArbolEspejo :: Arbol Int -> Bool
prop_mapArbolEspejo = undefined

-- La comprobación es
--   > quickCheck prop_mapArbolEspejo

-- ----------------------------------------------------------------------------
-- Ejercicio 28.3. Comprobar con QuickCheck que
--   (map (1+)) . preorden = preorden . (mapArbol (1+))
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_mapPreorden :: Arbol Int -> Bool
prop_mapPreorden = undefined

-- La comprobación es
--   > quickCheck prop_mapPreorden

-- ============================================================================
-- Árboles genéricos
-- ============================================================================

-- ----------------------------------------------------------------------------
-- En los siguientes ejercicios se usará el tipo algebraico de datos de los
-- árboles genéricos definido por
--   data ArbolG t = NG t [ArbolG t]
--                   deriving (Show, Eq)
-- en la que un árbol se representa con el constructor H seguido del valor que
-- tiene el nodo raíz del árbol y de la lista de las representaciones de sus
-- árboles hijos.
--
-- Por ejemplo, los árboles
--      1               3
--     / \             /|\
--    2   3           / | \
--        |          5  4  7
--        4          |     /\
--                   6    2  1
-- se representan por
--   ej1, ej2 :: ArbolG Int
--   ej1 = NG 1 [NG 2 [], NG 3 [NG 4 []]]
--   ej2 = NG 3 [NG 5 [NG 6 []], NG 4 [], NG 7 [NG 2 [], NG 1 []]
--
-- En particular en esta representación una hoja es un árbol genérico con una
-- lista vacía de hijos: NG x [].
-- ----------------------------------------------------------------------------

data ArbolG t = NG t [ArbolG t]
                deriving (Show, Eq)

ej1, ej2 :: ArbolG Int
ej1 = NG 1 [NG 2 [], NG 3 [NG 4 []]]
ej2 = NG 3 [NG 5 [NG 6 []], NG 4 [], NG 7 [NG 2 [], NG 1 []]]

-- ----------------------------------------------------------------------------
-- Ejercicio 29. Definir la función
--   listaHojas :: ArbolG t -> [t]
-- tal que '(listaHojas a)' es la lista de los valores de las hojas del árbol
-- genérico a. Por ejemplo,
--   listaHojas ej1  ==  [2,4]
--   listaHojas ej2  ==  [6,4,2,1]
-- ----------------------------------------------------------------------------

listaHojas :: ArbolG t -> [t]
listaHojas = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 30. Definir la función
--   ramasLargas :: ArbolG t -> [[t]]
-- tal que '(ramasLargas a)' es la lista de las ramas más largas del árbol 'a'.
-- Por ejemplo,
--   ramasLargas ej1  ==  [[1,3,4]]
--   ramasLargas ej2  ==  [[3,5,6],[3,7,2],[3,7,1]]
-- ----------------------------------------------------------------------------

ramasLargas :: ArbolG t -> [[t]]
ramasLargas = undefined

-- ============================================================================
-- Generador de árboles binarios para los ejercicios con QuickCheck
-- ============================================================================

-- genArbol es un generador de árboles binarios de tamaño dado. El tamaño
-- indica el número máximo de nodos generados. Por ejemplo,
--   > sample (genArbol 3 :: Gen (Arbol Int))
--   N 0 (H (-1)) (H 0)
--   N (-1) (H (-2)) (H (-2))
--   H 3
--   N (-1) (H (-5)) (H (-4))
--   H 5
--   N 63 (N 35 (H 26) (H 0)) (N (-18) (H 50) (H 45))
--   H (-110)
--   N 231 (H 213) (N (-127) (H 175) (H (-183)))
--   H (-858)
--   H 124
--   H 3271

genArbol :: (Arbitrary t) => Int -> Gen (Arbol t)
genArbol 0 =
  do x <- oneof [arbitrary]
     return (H x)
genArbol n =
  do x <- oneof [arbitrary]
     i <- oneof [genArbol (div n 2)]
     d <- oneof [genArbol (div n 2)]
     e <- choose (True,False)
     if e then return (H x)
          else return (N x i d)

-- Los árboles binarios pertenencen a la clase de los objetos generables
-- aleatoriamente.

instance Arbitrary t => Arbitrary (Arbol t) where
  arbitrary = sized genArbol

-- ============================================================================
