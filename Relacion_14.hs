-- I1M: Relación 14
-- Algoritmo de Triangulación de Gauss
-- Departamento de Ciencias de la Computación e Inteligencia Artificial
-- Universidad de Sevilla
-- ============================================================================

-- ============================================================================
-- Librerías auxiliares
-- ============================================================================

import Data.Array
import Data.Ratio

-- ============================================================================
-- Vectores y matrices
-- ============================================================================

-- Los vectores son tablas cuyos índices son números naturales.

type Vector a = Array Int a

-- Las matrices son tablas cuyos índices son pares de números naturales.

type Matriz a = Array (Int,Int) a

-- ----------------------------------------------------------------------------
-- Se considera la función
--   dimension :: Matriz a -> (Int,Int)
-- tal que '(dimension m)' es el par formado por el número de filas y el número
-- de columnas de la matriz 'm'. Por ejemplo,
--   dimension (listaMatriz [[1,3,5],[2,4,7]])  ==  (2,3)
-- ----------------------------------------------------------------------------

dimension :: Num a => Matriz a -> (Int,Int)
dimension = snd . bounds

-- ----------------------------------------------------------------------------
-- Se considera la función
--   listaMatriz :: [[a]] -> Matriz a
-- tal que '(listaMatriz xss)' es la matriz cuyas filas son los elementos de
-- 'xss', en el orden en que aparecen. Por ejemplo,
--   listaMatriz [[1,3,5],[2,4,7]]  ==
--     array ((1,1),(2,3)) [((1,1),1),((1,2),3),((1,3),5),
--                          ((2,1),2),((2,2),4),((2,3),7)]
-- ----------------------------------------------------------------------------

listaMatriz :: [[a]] -> Matriz a
listaMatriz xss =
  listArray ((1,1),(length xss,length (xss!!0))) (concat xss)

-- ============================================================================
-- Operaciones elementales con filas y columnas
-- ============================================================================

-- ----------------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--   intercambiaFilas :: Num a => Int -> Int -> Matriz a -> Matriz a
-- tal que '(intercambiaFilas f1 f2 m)' es la matriz obtenida intercambiando
-- las filas 'f1' y 'f2' de la matriz 'm'. Por ejemplo,
--   intercambiaFilas 1 3 (listaMatriz [[5,1,0],[3,2,6],[4,6,9]])  ==
--     array ((1,1),(3,3)) [((1,1),4),((1,2),6),((1,3),9),
--                          ((2,1),3),((2,2),2),((2,3),6),
--                          ((3,1),5),((3,2),1),((3,3),0)]
-------------------------------------------------------------------------

intercambiaFilas :: Num a => Int -> Int -> Matriz a -> Matriz a
intercambiaFilas = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--   intercambiaColumnas :: Num a => Int -> Int -> Matriz a -> Matriz a
-- tal que '(intercambiaColumnas c1 c2 m)' es la matriz obtenida intercambiando
-- las columnas 'c1' y 'c2' de la matriz 'm'. Por ejemplo,
--   intercambiaColumnas 1 3 (listaMatriz [[5,1,0],[3,2,6],[4,6,9]])  ==
--     array ((1,1),(3,3)) [((1,1),0),((1,2),1),((1,3),5),
--                          ((2,1),6),((2,2),2),((2,3),3),
--                          ((3,1),9),((3,2),6),((3,3),4)]
-- ----------------------------------------------------------------------------

intercambiaColumnas :: Num a => Int -> Int -> Matriz a -> Matriz a
intercambiaColumnas = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--   multiplicaFila :: Num a => Int -> a -> Matriz a -> Matriz a
-- tal que '(multiplicaFila f x m)' es la matriz obtenida multiplicando la fila
-- 'f' de la matriz 'm' por el número 'x'. Por ejemplo,
--   multiplicaFila 2 3 (listaMatriz [[5,1,0],[3,2,6],[4,6,9]])  ==
--     array ((1,1),(3,3)) [((1,1),5),((1,2),1),((1,3), 0),
--                          ((2,1),9),((2,2),6),((2,3),18),
--                          ((3,1),4),((3,2),6),((3,3), 9)]
-- ----------------------------------------------------------------------------

multiplicaFila :: Num a => Int -> a -> Matriz a -> Matriz a
multiplicaFila = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--   multiplicaColumna :: Num a => Int -> a -> Matriz a -> Matriz a
-- tal que '(multiplicaColumna c x m)' es la matriz obtenida multiplicando la
-- columna 'c' de la matriz 'm' por el número 'x'. Por ejemplo,
--   multiplicaColumna 2 3 (listaMatriz [[5,1,0],[3,2,6],[4,6,9]])  ==
--     array ((1,1),(3,3)) [((1,1),5),((1,2), 3),((1,3),0),
--                          ((2,1),3),((2,2), 6),((2,3),6),
--                          ((3,1),4),((3,2),18),((3,3),9)]
-- ----------------------------------------------------------------------------

multiplicaColumna :: Num a => Int -> a -> Matriz a -> Matriz a
multiplicaColumna = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--   sumaFilas :: Num a => Int -> Int -> Matriz a -> Matriz a
-- tal que '(sumaFilas f1 f2 m)' es la matriz obtenida sumando la fila 'f2' a
-- la fila 'f1' en la matriz 'm'. Por ejemplo,
--   sumaFilas 2 3 (listaMatriz [[5,1,0],[3,2,6],[4,6,9]])  ==
--     array ((1,1),(3,3)) [((1,1),5),((1,2),1),((1,3), 0),
--                          ((2,1),7),((2,2),8),((2,3),15),
--                          ((3,1),4),((3,2),6),((3,3), 9)]
-- ----------------------------------------------------------------------------

sumaFilas :: Num a => Int -> Int -> Matriz a -> Matriz a
sumaFilas = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--   sumaColumnas :: Num a => Int -> Int -> Matriz a -> Matriz a
-- tal que '(sumaColumnas c1 c2 m)' es la matriz obtenida sumando la columna
-- 'c2' a la columna 'c1' en la matriz 'm'. Por ejemplo,
--   sumaColumnas 2 3 (listaMatriz [[5,1,0],[3,2,6],[4,6,9]])  ==
--     array ((1,1),(3,3)) [((1,1),5),((1,2), 1),((1,3),0),
--                          ((2,1),3),((2,2), 8),((2,3),6),
--                          ((3,1),4),((3,2),15),((3,3),9)]
-- ----------------------------------------------------------------------------

sumaColumnas :: Num a => Int -> Int -> Matriz a -> Matriz a
sumaColumnas = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--   sumaFilaPor :: Num a => Int -> Int -> a -> Matriz a -> Matriz a
-- tal que '(sumaFilaPor f1 f2 x m)' es la matriz obtenida sumando la fila 'f2'
-- multiplicada por 'x' a la fila 'f1' en la matriz 'm'. Por ejemplo,
--   sumaFilaPor 2 3 10 (listaMatriz [[5,1,0],[3,2,6],[4,6,9]])  ==
--     array ((1,1),(3,3)) [((1,1), 5),((1,2), 1),((1,3), 0),
--                          ((2,1),43),((2,2),62),((2,3),96),
--                          ((3,1), 4),((3,2), 6),((3,3), 9)]
-- ----------------------------------------------------------------------------

sumaFilaPor :: Num a => Int -> Int -> a -> Matriz a -> Matriz a
sumaFilaPor = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 8. Definir la función
--   sumaColumnaPor :: Num a => Int -> Int -> a -> Matriz a -> Matriz a
-- tal que '(sumaColumnaPor c1 c2 x m)' es la matriz obtenida sumando la
-- columna 'c2' multiplicada por 'x' a la columna 'c1' en la matriz 'm'. Por
-- ejemplo,
--   sumaColumnaPor 2 3 10 (listaMatriz [[5,1,0],[3,2,6],[4,6,9]])  ==
--     array ((1,1),(3,3)) [((1,1),5),((1,2), 1),((1,3),0),
--                          ((2,1),3),((2,2),62),((2,3),6),
--                          ((3,1),4),((3,2),96),((3,3),9)]
-- ----------------------------------------------------------------------------

sumaColumnaPor :: Num a => Int -> Int -> a -> Matriz a -> Matriz a
sumaColumnaPor = undefined

-- ============================================================================
-- Método de Triangulación de Gauss
-- ============================================================================

-- ----------------------------------------------------------------------------
-- El método de triangulación de Gauss consiste en realizar operaciones
-- elementales sobre una matriz hasta convertirla en una matriz triangular
-- superior. Esta transformación se puede utilizar para calcular el rango de
-- una matriz, el determinante de una matriz cuadrada, la inversa de una matriz
-- regular o para resolver sistemas de ecuaciones lineales.
--
-- Dada una matriz A, el método comienza buscando un elemento no nulo en dicha
-- matriz y moviéndolo a la posicion (1,1). Este elemento se llama el pivote de
-- la primera columna y se busca en una fila y una columna mayores o iguales a
-- las de la posición (1,1). Para hacer esto se procede de la siguiente forma:
-- · Se localiza una columna mayor o igual que 1, en la que haya un elemento no
--   nulo en una fila mayor o igual que 1.
-- · Se intercambia dicha columna con la columna 1.
-- · Se localiza una fila mayor o igual que 1, en la que haya un elemento no
--   nulo en la columna 1.
-- · Se intercambia dicha fila con la fila 1.
-- Por ejemplo, dada la matriz
--
--               / 0  0  1 \
--               | 0  4  8 |
--               \ 0  8  9 /
--
-- intercambiamos las columnas 1 y 2 y obtenemos
--
--               / 0  0  8 \
--               | 4  0  1 |
--               \ 8  0  9 /
--
-- a continuación intercambiamos las filas 1 y 2 y obtenemos
--
--               / 4  0  1 \
--               | 0  0  8 |
--               \ 8  0  9 /
--
-- A continuación se procede a anular los elementos que están en la primera
-- columna por debajo de la diagonal principal mediante operaciones elementales
-- en las que a cada fila se le suma o resta un múltiplo de la primera. Por
-- ejemplo, dada la matriz
--
--               /  2  2  1 \
--               |  6  8  9 |
--               \ -4  4  3 /
--
-- tenemos que
-- · Restar a la segunda fila el triple de la primera
-- · Sumar a la tercera fila el doble de la primera
-- y obtenemos:
--               / 2  2  1 \
--               | 0  2  6 |
--               \ 0  8  5 /
--
-- A continuación se continua el proceso a partir del segundo elemento de la
-- diagonal principal, posición (2,2), y así hasta llegar al último elemento de
-- la diagonal principal. En el ejemplo, bastaría con restar a la tercera fila
-- el cuadruple de la segunda, obteniendo:
--
--               / 2  2   1 \
--               | 0  2   6 |
--               \ 0  0 -19 /
--
-- ----------------------------------------------------------------------------

-- ----------------------------------------------------------------------------
-- Ejercicio 9. Definir la función
--   matrizNoNulaDesde :: (Num a, Eq a) => Matriz a -> Int -> Int -> Bool
-- tal que '(matrizNoNulaDesde m f c)' se verifica si la matriz 'm' tiene una
-- columna a partir de la columna 'c' con algún elemento no nulo a partir de la
-- fila 'f'; es decir, si la submatriz de 'm' obtenida eliminando las 'f-1'
-- primeras filas y las 'c-1' primeras columnas es no nula. Por ejemplo,
--   matrizNoNulaDesde (listaMatriz [[3,2,5],[5,0,0],[6,0,0]]) 2 2  ==  False
--   matrizNoNulaDesde (listaMatriz [[3,2,5],[5,7,0],[6,0,0]]) 2 2  ==  True
-- ----------------------------------------------------------------------------

matrizNoNulaDesde :: (Num a, Eq a) => Matriz a -> Int -> Int -> Bool
matrizNoNulaDesde = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 10. Definir la función
--   columnaNoNulaDesde :: (Num a, Eq a) => Matriz a -> Int -> Int -> Bool
-- tal que '(columnaNoNulaDesde m f c)' se verifica si la matriz 'm' tiene
-- algún elemento no nulo en la columna 'c' a partir de la fila 'f'. Por
-- ejemplo,
--   columnaNoNulaDesde (listaMatriz [[3,2],[5,1],[0,4]]) 2 1  ==  True
--   columnaNoNulaDesde (listaMatriz [[3,2],[5,0],[0,0]]) 2 2  ==  False
-- ----------------------------------------------------------------------------

columnaNoNulaDesde :: (Num a, Eq a) => Matriz a -> Int -> Int -> Bool
columnaNoNulaDesde = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 11. Definir la función
--   indiceColumnaNoNulaDesde :: (Num a, Eq a) =>
--                               Matriz a -> Int -> Int -> Maybe Int
-- tal que '(indiceColumnaNoNulaDesde m f c)' es el índice de la primera
-- columna, a partir de la columna 'c', en la que la matriz 'm' tiene un
-- elemento no nulo a partir de la fila 'f'. Por ejemplo,
--   indiceColumnaNoNulaDesde (listaMatriz [[3,2,5],[5,7,0],[6,0,0]]) 2 2  ==
--     Just 2
--   indiceColumnaNoNulaDesde (listaMatriz [[3,2,5],[5,0,0],[6,0,2]]) 2 2  ==
--     Just 3
--   indiceColumnaNoNulaDesde (listaMatriz [[3,2,5],[5,0,0],[6,0,0]]) 2 2  ==
--     Nothing
-- ----------------------------------------------------------------------------

indiceColumnaNoNulaDesde :: (Num a, Eq a) =>
                            Matriz a -> Int -> Int -> Maybe Int
indiceColumnaNoNulaDesde = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 12. Definir la función
--   indiceFilaNoNulaDesde :: (Num a, Eq a) =>
--                            Matriz a -> Int -> Int -> Maybe Int
-- tal que '(indiceFilaNoNulaDesde m f c)' es el menor índice 'k', mayor o
-- igual que 'f', tal que el elemento de la matriz 'm' en la posición '(k,c)'
-- es no nulo. Por ejemplo,
--   indiceFilaNoNulaDesde (listaMatriz [[5,1,0],[3,2,6],[4,6,9]]) 2 3  ==
--     Just 2
--   indiceFilaNoNulaDesde (listaMatriz [[5,1,1],[3,2,0],[4,6,0]]) 2 3  ==
--     Nothing
-- ----------------------------------------------------------------------------

indiceFilaNoNulaDesde :: (Num a, Eq a) => Matriz a -> Int -> Int -> Maybe Int
indiceFilaNoNulaDesde = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 13. Definir la función
--   anulaColumnaDesde :: (Fractional a, Eq a) =>
--                         Matriz a -> Int -> Int -> Matriz a
-- tal que '(anulaColumnaDesde m f c)' es la matriz obtenida anulando todos los
-- elementos de la columna 'c' de la matriz 'm' por debajo de la posición
-- '(f,c)' (se supone que el elemento en la posición '(f,c)' no es nulo). Por
-- ejemplo,
--   anulaColumnaDesde (listaMatriz [[2.0,2,1],[2,4,8],[10,8,9]]) 1 2  ==
--     array ((1,1),(3,3)) [((1,1), 2.0),((1,2),2.0),((1,3),1.0),
--                          ((2,1),-2.0),((2,2),0.0),((2,3),6.0),
--                          ((3,1), 2.0),((3,2),0.0),((3,3),5.0)]
--   anulaColumnaDesde (listaMatriz [[4,5],[2,7%2],[6,10]]) 1 1        ==
--     array ((1,1),(3,2)) [((1,1),4 % 1),((1,2),5 % 1),
--                          ((2,1),0 % 1),((2,2),1 % 1),
--                          ((3,1),0 % 1),((3,2),5 % 2)]
-- ----------------------------------------------------------------------------

anulaColumnaDesde :: (Fractional a, Eq a) =>
                     Matriz a -> Int -> Int -> Matriz a
anulaColumnaDesde = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 14. Definir la función
--   gaussAux :: (Fractional a, Eq a) => Matriz a -> Int -> Matriz a
-- tal que '(gaussAux m n)' es la matriz obtenida a partir de la matriz 'm'
-- aplicando el método de triangulación de Gauss a partir de la 'n'-ésima
-- posición de la diagonal principal. El proceso es el siguiente:
--   1. Si 'n' es el último elemento de la diagonal principal, entonces se
--      devuelve 'm'.
--   2. Si la submatriz de 'm' sin las 'n-1' primeras filas y las 'n-1'
--      primeras columnas es nula, entonces se devuelve 'm'.
--   3. En caso contrario, se devuelve '(gaussAux m3 (n+1))' siendo
--   3.1. 'c1' la primera columna a partir de la 'n' donde 'm' tiene
--        algún elemento no nulo a partir de la fila 'n',
--   3.2. 'm1' la matriz obtenida intercambiando las columnas 'n' y 'c1'
--        de 'm',
--   3.3. 'f1' la primera fila a partir de la 'n' donde la columna 'n' de
--        'm1' tiene un elemento no nulo,
--   3.4. 'm2' la matriz obtenida intercambiando las filas 'n' e 'f1' de
--        la matriz 'm1' y
--   3.5. 'm3' la matriz obtenida anulando todos los elementos de la
--        columna 'n' de 'm2' por debajo de la fila 'n'.
-- Por ejemplo,
--   gaussAux (listaMatriz [[1.0,2,3],[1,2,4],[3,2,5]]) 2  ==
--     array ((1,1),(3,3)) [((1,1),1.0),((1,2),2.0),((1,3),3.0),
--                          ((2,1),1.0),((2,2),2.0),((2,3),4.0),
--                          ((3,1),2.0),((3,2),0.0),((3,3),1.0)]
-- ----------------------------------------------------------------------------

gaussAux :: (Fractional a, Eq a) => Matriz a -> Int -> Matriz a
gaussAux = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 15. Definir la función
--   gauss :: (Fractional a, Eq a) => Matriz a -> Matriz a
-- tal que '(gauss m)' es la triangulación de la matriz 'm' por el método de
-- Gauss. Por ejemplo,
--   gauss (listaMatriz [[1.0,2,3],[1,2,4],[1,2,5]])  ==
--     array ((1,1),(3,3)) [((1,1),1.0),((1,2),3.0),((1,3),2.0),
--                          ((2,1),0.0),((2,2),1.0),((2,3),0.0),
--                          ((3,1),0.0),((3,2),0.0),((3,3),0.0)]
--   gauss (listaMatriz [[3%1,2,3],[1,2,4],[1,2,5]])  ==
--     array ((1,1),(3,3)) [((1,1),3 % 1),((1,2),2 % 1),((1,3),3 % 1),
--                          ((2,1),0 % 1),((2,2),4 % 3),((2,3),3 % 1),
--                          ((3,1),0 % 1),((3,2),0 % 1),((3,3),1 % 1)]
--   gauss (listaMatriz [[1.0,0,3],[1,0,4],[3,0,5]])  ==
--     array ((1,1),(3,3)) [((1,1),1.0),((1,2),3.0),((1,3),0.0),
--                          ((2,1),0.0),((2,2),1.0),((2,3),0.0),
--                          ((3,1),0.0),((3,2),0.0),((3,3),0.0)]
-- ----------------------------------------------------------------------------

gauss :: (Fractional a, Eq a) => Matriz a -> Matriz a
gauss = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 16. Definir la función
--   rango :: (Fractional a, Eq a) => Matriz a -> Int
-- tal que '(rango m)' es el rango de la matriz 'm' calculado haciendo uso del
-- método de triangulación de Gauss. Por ejemplo
--   rango (listaMatriz [[1.0,2,3],[1,2,4],[1,2,5]])   ==  2
--   rango (listaMatriz [[3.0,2,3],[1,2,4],[1,2,5]])   ==  3
--   rango (listaMatriz [[1.0,2,4],[2,4,8],[4,8,16]])  ==  1
-- ----------------------------------------------------------------------------

rango :: (Fractional a, Eq a) => Matriz a -> Int
rango = undefined

-- ============================================================================
-- Cálculo del determinante por el método de triangulación de Gauss
-- ============================================================================

-- ----------------------------------------------------------------------------
-- El cálculo del determinante de una matriz por el método de triangulación de
-- Gauss consiste en realizar operaciones elementales sobre la matriz hasta
-- convertirla en triangular superior. Como el determinante del producto de
-- matrices es igual al producto de los determinantes, entonces el determinante
-- de la matriz triangular será igual al producto de los determinantes de las
-- matrices que representan a las operaciones elementales utilizadas y el
-- determinante de la matriz original:
--
-- Dada una matriz cuadrada A, si la secuencia de operaciones elementales que
-- se usan para transformar A en una matriz triangular T son F1,...,Fk por
-- filas y C1,...,Ch por columnas:
--   Fk · ... · F2 · F1 · A · C1 · C2 · ... · Ch = T
-- entonces el determinante de T se relaciona con el de A de la siguiente
-- forma:
--   |Fk| · ... · |F2| · |F1| · |A| · |C1| · |C2| · ... · |Ch| = |T|
--
-- Las operaciones que consisten en sumar o restar a una fila (columna) un
-- múltiplo de otra se pueden expresar como el resultado de multiplicar por una
-- matriz con determinante 1.
--
-- El resultado de sumar a la segunda fila el doble de la primera se puede
-- obtener multiplicando por la izquierda por la matriz:
--
--   / 1  0  0 \
--   | 2  1  0 |        con determinante 1
--   \ 0  0  1 /
--
-- El resultado de restar a la tercera columna la mitad de la primera se puede
-- obtener multiplicando por la derecha por la matriz:
--
--   / 1  0  -1/2 \
--   | 0  1    0  |        con determinante 1
--   \ 0  0    1  /
--
-- Las operaciones que consisten en intercambiar dos filas o dos columnas se
-- pueden expresar como el resultado de multiplicar por una matriz con
-- determinante -1.
--
-- El resultado de intercambiar las filas segunda y tercera se puede obtener
-- multiplicando por la izquierda por la matriz:
--
--   / 1  0  0 \
--   | 0  0  1 |        con determinante -1
--   \ 0  1  0 /
--
-- El resultado de intercambiar las columnas primera y tercera se puede obtener
-- multiplicando por la derecha por la matriz:
--
--   / 0  0  1 \
--   | 0  1  0 |        con determinante -1
--   \ 1  0  0 /
--
-- El determinante de una matriz triangular (superior o inferior) es igual al
-- producto de los elementos de la diagonal principal.
--
-- De esta forma, para calcular el determinante de una matriz A, basta con
-- aplicar el proceso de triangulación de Gauss y 'anotar' el número de
-- intercambios de filas y de columnas. El determinante de A será igual al
-- producto de los elementos de la diagonal principal de la matriz triangular
-- resultante (|T|) por (-1) elevado al número de veces que se han hecho
-- intercambios de filas o de columnas.
-- ----------------------------------------------------------------------------

-- ----------------------------------------------------------------------------
-- Ejercicio 17. Definir la función
--   matrizIntercambiaFilas :: Num a => Int -> Int -> Matriz a -> Matriz a
-- tal que '(matrizIntercambiaFilas f1 f2 m)' es la matriz que multiplicando
-- a la izquierda por la matriz 'm' produce el efecto de intercambiar las filas
-- 'f1' y 'f2' en 'm'. Por ejemplo,
--   matrizIntercambiaFilas 1 2 (listaMatriz [[5,1],[3,2],[4,6]])  ==
--     array ((1,1),(3,3)) [((1,1),0),((1,2),1),((1,3),0),
--                          ((2,1),1),((2,2),0),((2,3),0),
--                          ((3,1),0),((3,2),0),((3,3),1)]
--   matrizIntercambiaFilas 1 3 (listaMatriz [[5,1],[3,2],[4,6]])  ==
--     array ((1,1),(3,3)) [((1,1),0),((1,2),0),((1,3),1),
--                          ((2,1),0),((2,2),1),((2,3),0),
--                          ((3,1),1),((3,2),0),((3,3),0)]
--   matrizIntercambiaFilas 2 3 (listaMatriz [[5,1],[3,2],[4,6]])  ==
--     array ((1,1),(3,3)) [((1,1),1),((1,2),0),((1,3),0),
--                          ((2,1),0),((2,2),0),((2,3),1),
--                          ((3,1),0),((3,2),1),((3,3),0)]
-- ----------------------------------------------------------------------------

matrizIntercambiaFilas :: Num a => Int -> Int -> Matriz a -> Matriz a
matrizIntercambiaFilas = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 18. Definir la función
--   matrizIntercambiaColumnas :: Num a => Int -> Int -> Matriz a -> Matriz a
-- tal que '(matrizIntercambiaColumnas c1 c2 m)' es la matriz que multiplicando
-- a la derecha por la matriz 'm' produce el efecto de intercambiar las
-- columnas 'c1' y 'c2' en 'm'. Por ejemplo,
--   matrizIntercambiaColumnas 1 2 (listaMatriz [[5,1,3],[2,4,6]])  ==
--     array ((1,1),(3,3)) [((1,1),0),((1,2),1),((1,3),0),
--                          ((2,1),1),((2,2),0),((2,3),0),
--                          ((3,1),0),((3,2),0),((3,3),1)]
--   matrizIntercambiaColumnas 1 3 (listaMatriz [[5,1,3],[2,4,6]])  ==
--     array ((1,1),(3,3)) [((1,1),0),((1,2),0),((1,3),1),
--                          ((2,1),0),((2,2),1),((2,3),0),
--                          ((3,1),1),((3,2),0),((3,3),0)]
--   matrizIntercambiaColumnas 2 3 (listaMatriz [[5,1,3],[2,4,6]])  ==
--     array ((1,1),(3,3)) [((1,1),1),((1,2),0),((1,3),0),
--                          ((2,1),0),((2,2),0),((2,3),1),
--                          ((3,1),0),((3,2),1),((3,3),0)]
-- ----------------------------------------------------------------------------

matrizIntercambiaColumnas :: Num a => Int -> Int -> Matriz a -> Matriz a
matrizIntercambiaColumnas = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 19. Definir la función
--   matrizSumaFilaPor :: Num a => Int -> Int -> a -> Matriz a -> Matriz a
-- tal que '(matrizSumaFilaPor f1 f2 x m)' es la matriz que multiplicando a la
-- izquierda por la matriz 'm' produce el efecto de sumar la fila 'f2'
-- multiplicada por 'x' a la fila 'f1' en la matriz 'm'. Por ejemplo,
--   matrizSumaFilaPor 2 1 2 (listaMatriz [[5,1],[3,2],[4,6]])     ==
--     array ((1,1),(3,3)) [((1,1),1),((1,2),0),((1,3),0),
--                          ((2,1),2),((2,2),1),((2,3),0),
--                          ((3,1),0),((3,2),0),((3,3),1)]
--   matrizSumaFilaPor 3 1 5 (listaMatriz [[5,1],[3,2],[4,6]])     ==
--     array ((1,1),(3,3)) [((1,1),1),((1,2),0),((1,3),0),
--                          ((2,1),0),((2,2),1),((2,3),0),
--                          ((3,1),5),((3,2),0),((3,3),1)]
--   matrizSumaFilaPor 3 2 (-3) (listaMatriz [[5,1],[3,2],[4,6]])  ==
--     array ((1,1),(3,3)) [((1,1),1),((1,2),0),((1,3),0),
--                          ((2,1),0),((2,2),1),((2,3),0),
--                          ((3,1),0),((3,2),-3),((3,3),1)]
-- ----------------------------------------------------------------------------

matrizSumaFilaPor :: Num a => Int -> Int -> a -> Matriz a -> Matriz a
matrizSumaFilaPor = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 20. Definir la función
--   matrizSumaColumnaPor :: Num a => Int -> Int -> a -> Matriz a -> Matriz a
-- tal que '(matrizSumaColumnaPor c1 c2 x m)' es la matriz que multiplicada a
-- la derecha por la matriz 'm' produce el efecto de sumar la columna 'c2'
-- multiplicada por 'x' a la columna 'c1' en la matriz 'm'. Por ejemplo,
--   matrizSumaColumnaPor 2 1 2 (listaMatriz [[5,1,3],[2,4,6]])     ==
--     array ((1,1),(3,3)) [((1,1),1),((1,2),2),((1,3),0),
--                          ((2,1),0),((2,2),1),((2,3),0),
--                          ((3,1),0),((3,2),0),((3,3),1)]
--   matrizSumaColumnaPor 3 1 5 (listaMatriz [[5,1,3],[2,4,6]])     ==
--     array ((1,1),(3,3)) [((1,1),1),((1,2),0),((1,3),5),
--                          ((2,1),0),((2,2),1),((2,3),0),
--                          ((3,1),0),((3,2),0),((3,3),1)]
--   matrizSumaColumnaPor 3 2 (-3) (listaMatriz [[5,1,3],[2,4,6]])  ==
--     array ((1,1),(3,3)) [((1,1),1),((1,2),0),((1,3),0),
--                          ((2,1),0),((2,2),1),((2,3),-3),
--                          ((3,1),0),((3,2),0),((3,3),1)]
-- ----------------------------------------------------------------------------

matrizSumaColumnaPor :: Num a => Int -> Int -> a -> Matriz a -> Matriz a
matrizSumaColumnaPor = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 21. Definir la función
--   determinanteGaussAux :: (Fractional a, Eq a) =>
--                           Matriz a -> Int -> Int -> (Int,Matriz a)
-- tal que '(determinanteGaussAux m n i)' es el par '(k,mm)', donde 'mm' es la
-- matriz obtenida a partir de la matriz 'm' aplicando el método de
-- triangulación de Gauss a partir de la 'n'-ésima posición de la diagonal
-- principal; y el valor 'k' es igual a 'i' más el número de intercambios entre
-- filas y el número de intercambios entre columnas que se han producido
-- durante el cálculo. Por ejemplo,
--   determinanteGaussAux (listaMatriz [[1.0,2,3],[1,2,4],[1,2,5]]) 1 0  ==
--     (1,array ((1,1),(3,3)) [((1,1),1.0),((1,2),3.0),((1,3),2.0),
--                             ((2,1),0.0),((2,2),1.0),((2,3),0.0),
--                             ((3,1),0.0),((3,2),0.0),((3,3),0.0)])
-- ----------------------------------------------------------------------------

determinanteGaussAux :: (Fractional a, Eq a) =>
                        Matriz a -> Int -> Int -> (Int,Matriz a)
determinanteGaussAux = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 22. Definir la función
--   determinanteGauss :: (Fractional a, Eq a) => Matriz a -> Matriz a
-- tal que '(determinanteGauss m)' es el par '(n,mm)', donde 'mm' es la
-- triangulación de la matriz 'm' por el método de Gauss y 'n' es el número
-- total de intercambios entre filas e intercambios entre columnas que se han
-- producido durante el cálculo. Por ejemplo,
--   determinanteGauss (listaMatriz [[1.0,2,3],[1,2,4],[1,2,5]])  ==
--     (1,array ((1,1),(3,3)) [((1,1),1.0),((1,2),3.0),((1,3),2.0),
--                             ((2,1),0.0),((2,2),1.0),((2,3),0.0),
--                             ((3,1),0.0),((3,2),0.0),((3,3),0.0)])
-- ----------------------------------------------------------------------------

determinanteGauss :: (Fractional a, Eq a) => Matriz a -> (Int,Matriz a)
determinanteGauss = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 23. Definir la función
--   determinante :: (Fractional a, Eq a) => Matriz a -> a
-- tal que '(determinante m)' es el determinante de la matriz 'm', calculado
-- usando el método de triangulación de Gauss. Por ejemplo,
--   determinante (listaMatriz [[1.0,2,3],[1,3,4],[1,2,5]])  ==  2.0
-- ----------------------------------------------------------------------------

determinante :: (Fractional a, Eq a) => Matriz a -> a
determinante = undefined

-- ============================================================================
-- Cálculo de la inversa por el método de Gauss-Jordan
-- ============================================================================

-- ----------------------------------------------------------------------------
-- El cálculo de la inversa de una matriz por el método de Gauss-Jordan
-- consiste en realizar operaciones elementales por filas sobre la matriz hasta
-- convertirla en la identidad. Si estas mismas operaciones elementales por
-- filas se aplican en el mismo orden sobre la matriz identidad, el resultado
-- será la inversa de la matriz original:
--
-- Dada una matriz cuadrada A, si la secuencia de operaciones elementales por
-- filas F1,...,Fk transforma la matriz A en la identidad:
--   Fk · F(k-1) · F(k-2) · ... · F2 · F1 · A = Id
-- entonces dicha secuencia de operaciones elementales evaluadas sobre la
-- matriz identidad debe proporcionar la inversa de la matriz A:
--   Fk · F(k-1) · F(k-2) · ... · F2 · F1 · Id = A⁻¹
--
-- Para transformar una matriz cuadrada A en la matriz identidad se aplica un
-- proceso similar al método de triangulación de Gauss en el que no sólo se
-- eliminan los elementos que hay por debajo de la diagonal principal, también
-- se eliminan los elementos por encima de la diagonal principal y se reducen a
-- la unidad los elementos de dicha diagonal. Todo esto mediante operaciones
-- elementales por filas, es decir, no se debe realizar ninguna operación por
-- columnas (en el método de triangulación de Gauss se realizan intercambios
-- de columnas para posicionar elementos no nulos en la diagonal principal). Si
-- en algún momento del proceso no se pueden realizar más operaciones
-- elementales por filas para transformar la matriz A en la matriz identidad
-- (en esta situación el método de triangulación de Gauss realizaría un
-- intercambio de columnas), eso quiere decir que la submatriz que hay desde
-- ese punto de la diagonal tiene una columna nula y por tanto su determinante
-- sería nulo; en esta situación la matriz original no tendría inversa.
--
-- Veamos un ejemplo del método de Gauss-Jordan para calcular la inversa de una
-- matriz:
--                    / 2  2  1 \                     / 1  0  0 \
-- Dada la matriz A = | 2  4  8 |   y la auxiliar B = | 0  1  0 |
--                    \ 10 8  9 /                     \ 0  0  1 /
--
-- Como el elemento de la posición (1,1) no es nulo, se utiliza para eliminar
-- los elementos de dicha columna de la siguiente forma:
-- · Se resta a la segunda fila la primera
-- · Se resta a la tercera fila la primera multiplicada por 5
-- · Se divide la primera fila por 2
-- El resultado de estas operaciones en A y en B es el siguiente:
--
--                    / 1   1  1/2 \                   /  1  0  0 \
--             A⁽¹⁾ = | 0   2   7  |            B⁽¹⁾ = | -1  1  0 |
--                    \ 0  -2   4  /                   \ -5  0  1 /
--
-- Como el elemento de la posición (2,2) no es nulo, se utiliza para eliminar
-- los elementos de dicha columna de la siguiente forma:
-- · Se resta a la primera fila la mitad de la segunda
-- · Se suma a la tercera fila la primera
-- · Se divide la segunda fila por 2
-- El resultado de estas operaciones en A y en B es el siguiente:
--
--                    / 1  0  -3  \                  /   1   -1/2  0 \
--             A⁽²⁾ = | 0  1  7/2 |           B⁽²⁾ = | -1/2   1/2  0 |
--                    \ 0  0  11  /                  \  -6     1   1 /
--
-- Finalmente como el elemento de la posición (3,3) no es nulo, se utiliza para
-- eliminar los elementos de dicha columna de la siguiente forma:
-- · Se suma a la primera fila la tercera multiplicada por 3/11
-- · Se resta a la segunda fila la tercera multiplicada por 7/22
-- · Se divide la tercera fila por 11
-- El resultado de estas operaciones en A y en B es el siguiente:
--
--                    / 1  0  0 \                  / -7/11 -5/22  3/11 \
--             A⁽³⁾ = | 0  1  0 |           B⁽³⁾ = | 31/22  2/11 -7/22 |
--                    \ 0  0  1 /                  \ -6/11  1/11  1/11 /
--
-- Finalmente la matriz B se transforma en la inversa de la matriz A; se puede
-- comprobar que A · B⁽³⁾ = B⁽³⁾ · A = Id
-- ----------------------------------------------------------------------------

-- ----------------------------------------------------------------------------
-- Ejercicio 24. Definir la función
--   identidad :: (Num a) => Int -> Matriz a
-- tal que '(identidad n)' es la matriz identidad de 'n' filas y 'n' columnas.
-- Por ejemplo,
--   identidad 2  ==
--     array ((1,1),(2,2)) [((1,1),1),((1,2),0),
--                          ((2,1),0),((2,2),1)]
--   identidad 3  ==
--     array ((1,1),(3,3)) [((1,1),1),((1,2),0),((1,3),0),
--                          ((2,1),0),((2,2),1),((2,3),0),
--                          ((3,1),0),((3,2),0),((3,3),1)]
-- ----------------------------------------------------------------------------

identidad :: (Num a) => Int -> Matriz a
identidad = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 25. Definir la función
--   esIdentidad :: (Num a) => Matriz a -> Bool
-- tal que '(esIdentidad m)' se verifica si 'm' es una matriz identidad. Por
-- ejemplo,
--   esIdentidad (listaMatriz [[1,2,3],[2,3,4],[3,4,5]])  ==  False
--   esIdentidad (listaMatriz [[1,0,0],[0,3,0],[0,0,5]])  ==  False
--   esIdentidad (listaMatriz [[2,0,0],[0,2,0],[0,0,2]])  ==  False
--   esIdentidad (identidad 3)                            ==  True
-- ----------------------------------------------------------------------------

esIdentidad :: (Num a, Eq a) => Matriz a -> Bool
esIdentidad = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 26. Definir la función
--   anulaColumnaTotal :: (Fractional a, Eq a) =>
--                        Matriz a -> Int -> Int -> Matriz a
-- tal que '(anulaColumnaTotal m f c)' es la matriz obtenida anulando todos los
-- elementos de la columna 'c' de la matriz 'm' fuera de la posición '(f,c)'
-- (se supone que el elemento en la posición '(f,c)' no es nulo), y reduciendo
-- a la unidad el elemento de la posición '(f,c)', todo mediante operaciones
-- elementales por filas. Por ejemplo,
--   anulaColumnaTotal (listaMatriz [[2.0,2,1],[2,4,8],[10,8,9]]) 1 2  ==
--     array ((1,1),(3,3)) [((1,1), 1.0),((1,2),1.0),((1,3),0.5),
--                          ((2,1),-2.0),((2,2),0.0),((2,3),6.0),
--                          ((3,1), 2.0),((3,2),0.0),((3,3),5.0)]
--   anulaColumnaTotal (listaMatriz [[2.0,2,1],[2,4,8],[10,8,9]]) 2 2  ==
--     array ((1,1),(3,3)) [((1,1),1.0),((1,2),0.0),((1,3),-3.0),
--                          ((2,1),0.5),((2,2),1.0),((2,3), 2.0),
--                          ((3,1),6.0),((3,2),0.0),((3,3),-7.0)]
--   anulaColumnaTotal (listaMatriz [[2.0,2,1],[2,4,8],[10,8,9]]) 3 2  ==
--     array ((1,1),(3,3)) [((1,1),-0.5 ),((1,2),0.0),((1,3),-1.25 ),
--                          ((2,1),-3.0 ),((2,2),0.0),((2,3), 3.5  ),
--                          ((3,1), 1.25),((3,2),1.0),((3,3), 1.125)]
-- ----------------------------------------------------------------------------

anulaColumnaTotal :: (Fractional a, Eq a) =>
                     Matriz a -> Int -> Int -> Matriz a
anulaColumnaTotal = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 27. Definir la función
--   anulaColumnaTotalGJ :: (Fractional a, Eq a) =>
--                          ParMatriz a -> Int -> Int -> ParMatriz a
-- tal que '(anulaColumnaTotalGJ (m,w) f c)' es el par de matrices formado por
-- la matriz obtenida anulando todos los elementos de la columna 'c' de la
-- matriz 'm' fuera de la posición '(f,c)' (se supone que el elemento en la
-- posición '(f,c)' no es nulo), y reduciendo a la unidad el elemento de la
-- posición '(f,c)', todo mediante operaciones elementales por filas; y la
-- matriz obtenida realizando las mismas operaciones elementales sobre la
-- matriz 'w'. Por ejemplo,
--   anulaColumnaTotalGJ (listaMatriz [[2.0,2,1],[2,4,8],[10,8,9]],
--                        identidad 3) 2 2  ==
--     (array ((1,1),(3,3)) [((1,1),1.0),((1,2),0.0),((1,3),-3.0),
--                           ((2,1),0.5),((2,2),1.0),((2,3), 2.0),
--                           ((3,1),6.0),((3,2),0.0),((3,3),-7.0)],
--      array ((1,1),(3,3)) [((1,1),1.0),((1,2),-0.5 ),((1,3),0.0),
--                           ((2,1),0.0),((2,2), 0.25),((2,3),0.0),
--                           ((3,1),0.0),((3,2),-2.0 ),((3,3),1.0)])
-- ----------------------------------------------------------------------------

type ParMatriz a = (Matriz a, Matriz a)

anulaColumnaTotalGJ :: (Fractional a, Eq a) =>
                       ParMatriz a -> Int -> Int -> ParMatriz a
anulaColumnaTotalGJ = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 28. Definir la función
--   inversaGaussJordan :: (Fractional a, Eq a) => Matriz a -> Matriz a
-- tal que '(inversaGaussJordan m)' es la inversa de la matriz 'm' calculada
-- mediante el método de Gauss-Jordan. En caso de que la matriz 'm' no sea
-- cuadrada o no sea invertible, se deben indicar mensajes de error. Por
-- ejemplo,
--   inversaGaussJordan (listaMatriz [[2%1,2%1,1%1],
--                                    [2%1,4%1,8%1]])   =>
--     *** Exception: La matriz no es cuadrada
--   inversaGaussJordan (listaMatriz [[2%1,2%1,1%1],
--                                    [2%1,4%1,8%1],
--                                    [0%1,2%1,7%1]])   =>
--     *** Exception: La matriz no es invertible
--   inversaGaussJordan (listaMatriz [[2%1,2%1,1%1],
--                                    [2%1,4%1,8%1],
--                                    [10%1,8%1,9%1]])  ==
--     array ((1,1),(3,3)) [((1,1),(-7)%11),((1,2),(-5)%22),((1,3),   3%11),
--                          ((2,1),  31%22),((2,2),   2%11),((2,3),(-7)%22),
--                          ((3,1),(-6)%11),((3,2),   1%11),((3,3),   1%11)]
-- ----------------------------------------------------------------------------

inversaGaussJordan :: (Fractional a, Eq a) => Matriz a -> Matriz a
inversaGaussJordan = undefined

-- ============================================================================
-- Resolución de sistemas de ecuaciones lineales por eliminación
-- ============================================================================

-- ----------------------------------------------------------------------------
-- El método de eliminación de Gauss-Jordan para resolver un sistema de
-- ecuaciones lineales consiste en realizar operaciones elementales por filas a
-- la matriz de los coeficientes del sistema de ecuaciones lineales hasta
-- convertirla en la identidad. Si estas mismas operaciones elementales por
-- filas se aplican en el mismo orden sobre la matriz columna formada por los
-- términos independentes del sistema de ecuaciones lineales, el resultado será
-- la solución del sistema.
--
-- Este método proporciona una solución a un sistema de ecuaciones lineales
-- siempre que éste sea compatible determinado, es decir, que tenga solución
-- única.
--
-- Dado un sistema de ecuaciones lineales, por ejemplo:
--
--             x + 3y - 2z = 5
--            3x + 5y + 6z = 7
--            2x + 4y + 3z = 8
--
-- Identificamos dos matrices en el sistema, la matriz de los coeficientes y la
-- matriz ampliada. En el ejemplo anterior:
--
--                                / 1  3 -2 \
--   Matriz de los coeficientes = | 3  5  6 |
--                                \ 2  4  3 /
--
--                     / 1  3 -2  5 \
--   Matriz ampliada = | 3  5  6  7 |
--                     \ 2  4  3  8 /
--
-- Para analizar la existencia de solución de un sistema de ecuaciones
-- lineales, se estudia el rango de la matriz de los coeficientes y el rango
-- de la matriz ampliada. De acuerdo con el número de soluciones, se distinguen
-- tres tipos de sistemas de ecuaciones lineales:
-- · Sistemas incompatibles - No tienen solución. En estos sistemas el rango de
--   la matriz de los coeficientes es distinto del rango de la matriz ampliada.
-- · Sistemas compatibles determinados - Tienen una única solución. En estos
--   sistemas el rango de la matriz de los coeficientes es igual al rango de la
--   matriz ampliada e igual al número de ecuaciones.
-- · Sistemas compatibles indeterminados - Tienen infinitas soluciones. En
--   estos sistemas el rango de la matriz de los coeficientes es igual al rango
--   de la matriz ampliada pero menor que el número de ecuaciones.
--
-- En el ejemplo anterior tanto el rango de la matriz de los coeficientes como
-- el rango de la matriz ampliada es 3, igual al número de ecuaciones. Por
-- tanto dicho sistema es compatible determinado.
--
-- Para calcular la solución de un sistema de ecuaciones lineales compatible
-- determinado por el método de eliminación de Gauss-Jordan, se realizan
-- operaciones elementales por filas para reducir la matriz de los coeficientes
-- a la identidad. Al aplicar estas mismas operaciones sobre la matriz
-- ampliada, obtenemos en la última columna la solución del sistema.
--
-- En el ejemplo anterior la matriz ampliada es:
--
--                     / 1   3  -2   5 \
--   Matriz ampliada = | 3   5   6   7 |
--                     \ 2   4   3   8 /
--
-- Como el elemento de la posición (1,1) no es nulo, se puede utilizar para
-- eliminar los elementos de la primera columna de la siguiente forma:
-- · Se resta a la segunda fila el triple de la primera
-- · Se resta a la tercera fila el doble de la primera
-- El resultado de estas operaciones es el siguiente:
--
--                     / 1   3  -2   5 \
--   Matriz ampliada = | 0  -4  12  -8 |
--                     \ 0  -2   7  -2 /
--
-- A continuación, como el elemento de la posición (2,2) no es nulo, se utiliza
-- para eliminar los elementos de la segunda columna de la siguiente forma:
-- · Se divide la segunda fila por -4
-- · Se resta a la primera fila el tripe de la segunda
-- · Se suma a la tercera fila el doble de la segunda
-- El resultado de estas operaciones es el siguiente:
--
--                     / 1   0   7  -1 \
--   Matriz ampliada = | 0   1  -3   2 |
--                     \ 0   0   1   2 /
--
-- Finalmente, como el elemento de la posición (3,3) no es nulo, se utiliza
-- para eliminar los elementos de la tercera columna de la siguiente forma:
-- · Se resta a la primera fila siete veces la tercera
-- · Se suma a la segunda fila el triple de la tercera
-- El resultado de estas operaciones es el siguiente:
--
--                     / 1   0   0 -15 \
--   Matriz ampliada = | 0   1   0   8 |
--                     \ 0   0   1   2 /
--
-- La solución del sistema de ecuaciones lineales es (x,y,z) = (-15,8,2).
-- ----------------------------------------------------------------------------

-- ----------------------------------------------------------------------------
-- Utilizaremos el tipo SistemaLineal para representar los sistemas de
-- ecuaciones lineales como una lista con listas formadas por los coeficientes
-- y el término independiente de cada una de las ecuaciones (las filas de la
-- matriz ampliada).

type SistemaLineal = [[Float]]

-- ----------------------------------------------------------------------------
-- Utilizaremos el tipo Clasificacion para distinguir los tres tipos de
-- sistemas de ecuaciones lineales de acuerdo con el número de soluciones:
-- · SI: Sistemas incompatibles
-- · SCD: Sistemas compatibles determinados
-- · SCI: Sistemas compatibles indeterminados

data Clasificacion = SI
                   | SCD
                   | SCI
                     deriving (Eq, Show)

-- ----------------------------------------------------------------------------
-- Ejercicio 29. Definir la función
--   clasificacionSistemaLineal :: SistemaLineal -> Clasificacion
-- tal que '(clasificacionSistemaLineal sl)' es SI si el sistema lineal 'sl' es
-- incompatible (sin solución), SCD si el sistema lineal 'sl' es compatible
-- determinado (una única solución) y SCI si el sistema lineal es compatible
-- indeterminado (infinitas soluciones). Por ejemplo,
--   clasificacionSistemaLineal [[1,2,1],[2,4,7]]  ==  SI
--   clasificacionSistemaLineal [[1,2,1],[1,4,2]]  ==  SCD
--   clasificacionSistemaLineal [[1,2,1],[2,4,2]]  ==  SCI
-- ----------------------------------------------------------------------------

clasificacionSistemaLineal :: SistemaLineal -> Clasificacion
clasificacionSistemaLineal = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 30. Definir la función
--   solucionSistemaLineal :: SistemaLineal -> [Float]
-- tal que '(solucionSistemaLineal sl)' es la solución del sistema lineal 'sl'
-- calculada por el método de eliminación de Gauss-Jordan, en caso de que dicho
-- sistema sea compatible determinado. En otro caso se devuelve un mensaje de
-- error indicando la razón. Por ejemplo
--   solucionSistemaLineal [[1,2,1],[2,4,7]]                 =>
--     *** Exception: El sistema no es compatible determinado
--   solucionSistemaLineal [[1,2,1],[1,4,2]]                 ==  [0.0,0.5]
--   solucionSistemaLineal [[1,2,1],[2,4,2]]                 =>
--     *** Exception: El sistema no es compatible determinado
--   solucionSistemaLineal [[1,3,-2,5],[3,5,6,7],[2,4,3,8]]  ==
--     [-15.0,8.0,2.0]
-- ----------------------------------------------------------------------------

solucionSistemaLineal :: SistemaLineal -> [Float]
solucionSistemaLineal = undefined

-- ============================================================================
