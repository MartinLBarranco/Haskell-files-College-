-- I1M: Relación 13
-- Vectores y matrices
-- Departamento de Ciencias de la Computación e Inteligencia Artificial
-- Universidad de Sevilla
-- ============================================================================

-- ============================================================================
-- Librerías auxiliares
-- ============================================================================

import Data.Array

-- ============================================================================
-- Vectores y matrices
-- ============================================================================

-- Los vectores son tablas cuyos índices son números naturales.

type Vector a = Array Int a

-- Las matrices son tablas cuyos índices son pares de números naturales.

type Matriz a = Array (Int,Int) a

-- ----------------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--   listaVector :: [a] -> Vector a
-- tal que '(listaVector xs)' es el vector cuyos elementos son los de la lista
-- 'xs', en el orden en que aparecen. Por ejemplo,
--   listaVector [3,2,5]  ==  array (1,3) [(1,3),(2,2),(3,5)]
-- ----------------------------------------------------------------------------

listaVector :: [a] -> Vector a
listaVector xs = array (1, (length xs)) [(i, xs!!(i-1)) | i<-[1..(length xs)]]

-- ----------------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--   listaMatriz :: [[a]] -> Matriz a
-- tal que '(listaMatriz xss)' es la matriz cuyas filas son los elementos de
-- 'xss', en el orden en que aparecen. Por ejemplo,
--   listaMatriz [[1,3,5],[2,4,7]]  ==
--     array ((1,1),(2,3)) [((1,1),1),((1,2),3),((1,3),5),
--                          ((2,1),2),((2,2),4),((2,3),7)]
-- ----------------------------------------------------------------------------

listaMatriz :: [[a]] -> Matriz a
listaMatriz xss = array ((1,1),(length xss, length (head xss)) ) [((i,j), ((xss!!(i-1))!!(j-1)) ) | i<-[1..(length xss)], j<-[1..length(head xss)]]

-- ----------------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--   numFilas :: Matriz a -> Int
-- tal que '(numFilas m)' es el número de filas de la matriz 'm'. Por ejemplo,
--   numFilas (listaMatriz [[1,3,5],[2,4,7]])  ==  2
-- ----------------------------------------------------------------------------

numFilas :: Matriz a -> Int
numFilas m = fst(snd (bounds m))

-- ----------------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--   numColumnas :: Matriz a -> Int
-- tal que '(numColumnas m)' es el número de columnas de la matriz 'm'. Por
-- ejemplo,
--   numColumnas (listaMatriz [[1,3,5],[2,4,7]])  ==  3
-- ----------------------------------------------------------------------------

numColumnas :: Matriz a -> Int
numColumnas m = snd(snd(bounds m))

-- ----------------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--   dimension :: Matriz a -> (Int,Int)
-- tal que '(dimension m)' es el par formado por el número de filas y el número
-- de columnas de la matriz 'm'. Por ejemplo,
--   dimension (listaMatriz [[1,3,5],[2,4,7]])  ==  (2,3)
-- ----------------------------------------------------------------------------

dimension :: Matriz a -> (Int,Int)
dimension m = snd(bounds m)

-- ----------------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--   vectorLista :: Vector a -> [a]
-- tal que '(vectorLista v)' es la lista de los elementos del vector 'v'. Por
-- ejemplo,
--   vectorLista (array (1,3) [(1,3),(2,2),(3,5)])  ==  [3,2,5]
-- ----------------------------------------------------------------------------

vectorLista :: Vector a -> [a]
vectorLista v = elems v

-- ----------------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--   separa :: Int -> [a] -> [[a]]
-- tal que '(separa n xs)' es la lista obtenida separando los elementos de la
-- list 'xs' en grupos de 'n' elementos (salvo el último que puede tener menos
-- de 'n' elementos). Por ejemplo,
--   separa 3 [1..11]  ==  [[1,2,3],[4,5,6],[7,8,9],[10,11]]
-- ----------------------------------------------------------------------------

separa :: Int -> [a] -> [[a]]
separa n [] = [[]]
separa n xs | length xs <= n = [xs]
            | otherwise = [(take n xs)] ++separa n (drop n xs) 

-- ----------------------------------------------------------------------------
-- Ejercicio 8. Definir la función
--   matrizLista :: Matriz a -> [[a]]
-- tal que '(matrizLista m)' es la lista de las filas de la matriz 'm'. Por
-- ejemplo,
--   matrizLista (array ((1,1),(2,3)) [((1,1),5),((1,2),1),((1,3),0),
--                                     ((2,1),3),((2,2),2),((2,3),6)])  ==
--     [[5,1,0],[3,2,6]]
-- ----------------------------------------------------------------------------

matrizLista :: Matriz a -> [[a]]
matrizLista m = separa (numColumnas m) (elems m)

-- ----------------------------------------------------------------------------
-- Ejercicio 9. Definir la función
--   sumaMatrices :: Num a => Matriz a -> Matriz a -> Matriz a
-- tal que '(sumaMatrices m1 m2)' es la matriz suma de las matrices 'm1' y
-- 'm2'. Por ejemplo,
--   matrizLista (sumaMatrices (listaMatriz [[5,1,0],[3,2,6]])
--                             (listaMatriz [[4,6,3],[1,5,2]]))  ==
--     [[9,7,3],[4,7,8]]
-- ----------------------------------------------------------------------------

sumaMatrices :: Num a => Matriz a -> Matriz a -> Matriz a
sumaMatrices m n | numFilas m == numFilas n && numColumnas m == numColumnas n = array (bounds m) [((i),(m!i + n!i)) | i<- indices m]
                 |otherwise = error("No es posible hacer la suma")

-- ----------------------------------------------------------------------------
-- Ejercicio 10. Definir la función
--   filaMat :: Int -> Matriz a -> Vector a
-- tal que '(filaMat i m)' es el vector correspondiente a la 'i'-ésima fila de
-- la matriz 'm'. Por ejemplo,
--   vectorLista (filaMat 2 (listaMatriz [[5,1,0],[3,2,6],[4,5,7]]))  ==
--     [3,2,6]
-- ----------------------------------------------------------------------------

filaMat :: Int -> Matriz a -> Vector a
filaMat n m = array (1, numColumnas m) [(j, m!(n,j)) | j<-[1..(numColumnas m)]]

-- ----------------------------------------------------------------------------
-- Ejercicio 11. Definir la función
--   columnaMat :: Int -> Matriz a -> Vector a
-- tal que '(columnaMat j m)' es el vector correspondiente a la 'j'-ésima
-- columna de la matriz 'm'. Por ejemplo,
--   vectorLista (columnaMat 2 (listaMatriz [[5,1,0],[3,2,6],[4,5,7]]))  ==
--     [1,2,5]
-- ----------------------------------------------------------------------------

columnaMat :: Int -> Matriz a -> Vector a
columnaMat n m = array (1, numFilas m) [(j, m!(j,n)) | j<-[1..(numFilas m)]]

-- ----------------------------------------------------------------------------
-- Ejercicio 12. Definir la función
--   prodEscalar :: Num a => Vector a -> Vector a -> a
-- tal que '(prodEscalar v1 v2)' es el producto escalar de los vectores 'v1' y
-- 'v2'. Por ejemplo,
--   prodEscalar (listaVector [3,1,10]) (listaVector [3,1,10])  ==  110
-- ----------------------------------------------------------------------------

prodEscalar :: Num a => Vector a -> Vector a -> a
prodEscalar v w= sum(zipWith (*) (vectorLista v) (vectorLista w))

-- ----------------------------------------------------------------------------
-- Ejercicio 13. Definir la función
--   prodEscalarMatriz :: Num a => a -> Matriz a -> Matriz a
-- tal que '(prodEscalarMatriz x m)' es la matriz resultado de multiplicar la
-- matriz 'm' por el número 'x'. Por ejemplo,
--   matrizLista (prodEscalarMatriz 2 (listaMatriz [[3,1],[2,4]]))  ==
--     [[6,2],[4,8]]
-- ----------------------------------------------------------------------------
validas :: Matriz a -> Matriz a -> Bool
validas m p | numColumnas m == numFilas p = True
            | otherwise = False

producto :: Num a => Matriz a -> Matriz a -> Matriz a
producto m p = array ((1,1),(numFilas m, numColumnas p)) [( (i,j),(prodEscalar (filaMat i m) (columnaMat j p)) ) | i<-[1..numFilas m], j<-[1..numColumnas p]]

prodEscalarMatriz :: Num a => a -> Matriz a -> Matriz a
prodEscalarMatriz k m = array (bounds m) [(i, k*(m!i)) | i<- (indices m)]

-- ----------------------------------------------------------------------------
-- Ejercicio 14. Definir la función
--   prodMatrices :: Num a => Matriz a -> Matriz a -> Matriz a
-- tal que '(prodMatrices m1 m2)' es la matriz producto de las matrices 'm1' y
-- 'm2'. Por ejemplo,
--   matrizLista (prodMatrices (listaMatriz [[3,1],[2,4]])
--                             (listaMatriz [[3,1],[2,4]]))  ==
--     [[11,7],[14,18]]
--   matrizLista (prodMatrices (listaMatriz [[3,1],[2,4]])
--                             (listaMatriz [[7],[5]]))  ==
--     [[26],[34]]
-- ----------------------------------------------------------------------------

prodMatrices :: Num a => Matriz a -> Matriz a -> Matriz a
prodMatrices m p | validas m p = producto m p
                 | otherwise = error("No se pueden multiplicar")

-- ----------------------------------------------------------------------------
-- Ejercicio 15. Definir la función
--   potencia :: Num a => Matriz a -> Int -> Matriz a
-- tal que '(potencia m n)' es la potencia 'n'-ésima de la matriz cuadrada 'm'.
-- Por ejemplo,
--   matrizLista (potencia (listaMatriz [[1,1],[1,0]]) 2)  ==  [[2,1],[1,1]]
--   matrizLista (potencia (listaMatriz [[1,1],[1,0]]) 3)  ==  [[3,2],[2,1]]
--   matrizLista (potencia (listaMatriz [[1,1],[1,0]]) 4)  ==  [[5,3],[3,2]]
-- ¿Qué relación hay entre las potencias de la matriz de los ejemplos y la
-- sucesión de Fibonacci?
-- ----------------------------------------------------------------------------

potencia :: Num a => Matriz a -> Int -> Matriz a
potencia m 1 = m
potencia m n = prodMatrices m (potencia m (n-1))

-- ----------------------------------------------------------------------------
-- Ejercicio 16. Definir la función
--   traspuesta :: Matriz a -> Matriz a
-- tal que '(traspuesta m)' es la matriz traspuesta de la matriz 'm'. Por
-- ejemplo,
--   matrizLista (traspuesta (listaMatriz [[5,1,0],[3,2,6]]))  ==
--     [[5,3],[1,2],[0,6]]
-- ----------------------------------------------------------------------------

traspuesta :: Matriz a -> Matriz a
traspuesta m = array ((1,1),(numColumnas m, numFilas m)) [( (i,j),(m!(j,i) )) | i<-[1..numColumnas m], j<-[1..numFilas m]]

-- ----------------------------------------------------------------------------
-- Ejercicio 17. Definir la función
--   submatriz :: Int -> Int -> Matriz a -> Matriz a
-- tal que '(submatriz f c m)' es la matriz obtenida a partir de la matriz 'm'
-- eliminando la fila 'f' y la columna 'c'. Por ejemplo,
--   matrizLista (submatriz 2 3 (listaMatriz [[5,1,0],[3,2,6],[4,6,9]]))  ==
--     [[5,1],[4,6]]
-- ----------------------------------------------------------------------------

submatriz :: Int -> Int -> Matriz a -> Matriz a
submatriz f c m =
  let (p,q) = dimension m
  in array ((1,1),(p-1,q-1))
           [( (i,j) , (m!((fun i f),(fun j c))) ) |
            i <- [1..p-1], j <- [1..q-1]]

fun a b =
  if a < b
  then a
  else a+1


-- ----------------------------------------------------------------------------
-- Ejercicio 18. Definir la función
--   determinante :: Num a => Matriz a -> a
-- tal que '(determinante m)' es el determinante de la matriz 'm' calculado por
-- adjuntos. Por ejemplo,
--   determinante (listaMatriz [[2,0,0],[0,3,0],[0,0,1]])  ==  6
--   determinante (listaMatriz [[1,2,3],[4,5,6],[7,8,9]])  ==  0
--   determinante (listaMatriz [[2,1,5],[1,2,3],[5,4,2]])  ==  -33
-- ----------------------------------------------------------------------------

deter :: Num a => Matriz a -> a
deter m | numFilas m == 1 && numColumnas m == 1 = head(elems m)
        |otherwise = sum[((-1)^(i+1))*m!(1,i)*(determinante (submatriz 1 i m)) | i<-[1..numColumnas m]]



determinante :: Num a => Matriz a -> a
determinante m |esCuadrada m = deter m
               |otherwise = error("no se puede hayar el determinante")

-- ----------------------------------------------------------------------------
-- Ejercicio 19. Definir la función
--   esCuadrada :: Matriz a -> Bool
-- tal que '(esCuadrada m)' se verifica si la matriz 'm' es cuadrada. Por
-- ejemplo,
--   esCuadrada (listaMatriz [[5,1,0],[3,2,6]])  ==  False
--   esCuadrada (listaMatriz [[5,1],[3,2]])      ==  True
-- ----------------------------------------------------------------------------

esCuadrada :: Matriz a -> Bool
esCuadrada m = numFilas m == numColumnas m

-- ----------------------------------------------------------------------------
-- Ejercicio 20. Definir la función
--   esSimetrica :: Eq a => Matriz a -> Bool
-- tal que '(esSimetrica m)' se verifica si la matriz 'm' es simétrica. Por
-- ejemplo,
--   esSimetrica (listaMatriz [[5,1,3],[1,4,7],[3,7,2]])  ==  True
--   esSimetrica (listaMatriz [[5,1,3],[1,4,7],[3,4,2]])  ==  False
-- ----------------------------------------------------------------------------
iguales :: Eq a => Matriz a -> Matriz a -> Bool
iguales m p | dimension m == dimension p = and[m!i == p!i | i<-indices m]
            |otherwise = False


esSimetrica :: Eq a => Matriz a -> Bool
esSimetrica m = iguales m (traspuesta m)

-- ----------------------------------------------------------------------------
-- Ejercicio 21. Definir la función
--   esTriangularSuperior :: (Num a, Eq a) => Matriz a -> Bool
-- tal que '(esTriangularSuperior m)' se verifica si 'm' es una matriz
-- triangular superior. Por ejemplo,
--   esTriangularSuperior (listaMatriz [[1,2,1],[0,4,7],[0,0,5]])  ==  True
--   esTriangularSuperior (listaMatriz [[1,2,3],[1,2,4],[1,2,5]])  ==  False
-- ----------------------------------------------------------------------------
triangulo :: (Num a, Eq a) => Matriz a -> Bool
triangulo m = and[m!(i,j) == 0 | i<-[1..numFilas m], j<-[1..numColumnas m], j<i]



esTriangularSuperior :: (Num a, Eq a) => Matriz a -> Bool
esTriangularSuperior m | esCuadrada m = triangulo m
                       | otherwise = error("tpm")

-- ----------------------------------------------------------------------------
-- Ejercicio 22. Definir la función
--   esTriangularInferior :: (Num a, Eq a) => Matriz a -> Bool
-- tal que '(esTriangularInferior m)' se verifica si 'm' es una matriz
-- triangular inferior. Por ejemplo,
--   esTriangularInferior (listaMatriz [[1,0,0],[2,4,0],[1,2,5]])  ==  True
--   esTriangularInferior (listaMatriz [[1,2,3],[1,2,4],[1,2,5]])  ==  False
-- ----------------------------------------------------------------------------


esTriangularInferior :: (Num a, Eq a) => Matriz a -> Bool
esTriangularInferior m = esTriangularSuperior (traspuesta m)

-- ----------------------------------------------------------------------------
-- Ejercicio 23. Definir la función
--   esEscalar :: (Num a, Eq a) => Matriz a -> Bool
-- tal que '(esEscalar m)' se verifica si 'm' es una matriz escalar; es decir,
-- es una matriz diagonal con todos sus elementos iguales. Por ejemplo,
--   esEscalar (listaMatriz [[5,0,0],[0,5,0],[0,0,5]])  ==  True
--   esEscalar (listaMatriz [[5,0,0],[1,5,0],[0,0,5]])  ==  False
--   esEscalar (listaMatriz [[5,0,0],[0,6,0],[0,0,5]])  ==  False
-- ----------------------------------------------------------------------------

esEscalar :: (Num a, Eq a) => Matriz a -> Bool
esEscalar m = esTriangularInferior m && esTriangularSuperior m && and[m!(i,i) == m!(1,1) | i<-[1..(min (numFilas m) (numColumnas m))]]

-- ----------------------------------------------------------------------------
-- Ejercicio 24. Definir la función
--   diagonalPrincipal :: Matriz a -> Vector a
-- tal que '(diagonalPrincipal m)' es el vector que contiene los elementos de
-- la diagonal principal de la matriz 'm'. Por ejemplo,
--   vectorLista (diagonalPrincipal (listaMatriz [[5,1,0],[3,2,6]]))  ==
--     [5,2]
-- ----------------------------------------------------------------------------

diagonalPrincipal :: Matriz a -> Vector a
diagonalPrincipal m = let q = min (numFilas m) (numColumnas m) in
                      array (1,q) [(i, m!(i,i)) | i<-[1..q]]

-- ----------------------------------------------------------------------------
-- Ejercicio 25. Definir la función
--   diagonalSecundaria :: Matriz a -> Vector a
-- tal que '(diagonalSecundaria m)' es el vector que contiene los elementos de
-- la diagonal secundaria de la matriz 'm'. Por ejemplo,
--   vectorLista (diagonalSecundaria (listaMatriz [[5,1,0],[3,2,6]]))  ==
--     [1,3]
-- ----------------------------------------------------------------------------

diagonalSecundaria :: Matriz a -> Vector a
diagonalSecundaria = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 26. Definir la función
--   antidiagonal :: (Num a, Eq a) => Matriz a -> Bool
-- tal que '(antidiagonal m)' se verifica si 'm' es una matriz cuadrada y todos
-- sus elementos que no están en la diagonal secundaria son nulos. Por ejemplo,
--   antidiagonal (listaMatriz [[0,0,4],[0,6,0],[0,0,0]])  ==  True
--   antidiagonal (listaMatriz [[7,0,4],[0,6,0],[0,0,5]])  ==  False
-- ----------------------------------------------------------------------------

antidiagonal :: (Num a, Eq a) => Matriz a -> Bool
antidiagonal = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 27. Definir la función
--   posiciones :: Eq a => a -> Matriz a -> [(Int,Int)]
-- tal que '(posiciones x m)' es la lista de las posiciones de la matriz 'm'
-- cuyo valor es 'x'. Por ejemplo,
--   posiciones 2 (listaMatriz [[1,2,3],[2,4,6]])  ==  [(1,2),(2,1)]
--   posiciones 6 (listaMatriz [[1,2,3],[2,4,6]])  ==  [(2,3)]
--   posiciones 7 (listaMatriz [[1,2,3],[2,4,6]])  ==  []
-- ----------------------------------------------------------------------------

posiciones :: Eq a => a -> Matriz a -> [(Int,Int)]
posiciones n m = filter (\i -> m!i == n) (indices m)

-- ----------------------------------------------------------------------------
-- Ejercicio 28. Definir la función
--   indicesMaximo :: (Num a, Ord a) => Matriz a -> [(Int,Int)]
-- tal que '(indicesMaximo m)' es la lista de las posiciones en las que se
-- encuentra el elemento máximo de la matriz 'm'. Por ejemplo,
--   indicesMaximo (listaMatriz [[3,2],[3,1]])  ==  [(1,1),(2,1)]
-- ----------------------------------------------------------------------------

indicesMaximo :: (Num a, Ord a) => Matriz a -> [(Int,Int)]
indicesMaximo m = posiciones (maximum (elems m)) m

-- ----------------------------------------------------------------------------
-- Ejercicio 29. Una matriz tridiagonal es aquella en la que sólo hay elementos
-- distintos de 0 en la diagonal principal o en las diagonales por encima y por
-- debajo de la diagonal principal. Por ejemplo,
--   ( 1 2 0 0 0 0 )
--   ( 3 4 5 0 0 0 )
--   ( 0 6 7 8 0 0 )
--   ( 0 0 9 1 2 0 )
--   ( 0 0 0 3 4 5 )
--   ( 0 0 0 0 6 7 )
--
-- Definir la función
--   creaTridiagonal :: Int -> Matriz Int
-- tal que '(creaTridiagonal n)' es la siguiente matriz tridiagonal cuadrada
-- con 'n' filas y 'n' columnas:
--   ( 1 1 0 0 0 0 ... 0  0  )
--   ( 1 2 2 0 0 0 ... 0  0  )
--   ( 0 2 3 3 0 0 ... 0  0  )
--   ( 0 0 3 4 4 0 ... 0  0  )
--   ( 0 0 0 4 5 5 ... 0  0  )
--   ( 0 0 0 0 5 6 ... 0  0  )
--   ( ..................... )
--   ( 0 0 0 0 0 0 ... n  n  )
--   ( 0 0 0 0 0 0 ... n n+1 )
-- Por ejemplo,
--   matrizLista (creaTridiagonal 4)  ==
--     [[1,1,0,0],[1,2,2,0],[0,2,3,3],[0,0,3,4]]
-- ----------------------------------------------------------------------------

creaTridiagonal :: Int -> Matriz Int
creaTridiagonal = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 30. Definir la función
--   esTridiagonal :: (Num a, Eq a) => Matriz a -> Bool
-- tal que '(esTridiagonal m)' se verifica si la matriz 'm' es tridiagonal. Por
-- ejemplo,
--   esTridiagonal (creaTridiagonal 5)               ==  True
--   esTridiagonal (listArray ((1,1),(3,3)) [1..9])  ==  False
-- ----------------------------------------------------------------------------

esTridiagonal :: (Num a, Eq a) => Matriz a -> Bool
esTridiagonal = undefined

-- ============================================================================
