-- I1M: Relacion_04.hs
-- Definiciones por comprensión (II)
-- Departamento de Ciencias de la Computación e Inteligencia Artificial
-- Universidad de Sevilla
-- ============================================================================

-- ----------------------------------------------------------------------------
-- Ejercicio 1. Definir por comprensión la función
--   circulo :: Int -> Int
-- tal que '(circulo n)' es la cantidad de pares de números naturales '(x,y)'
-- que se encuentran dentro del círculo de radio 'n'. Por ejemplo,
--   circulo 3  ==  9
--   circulo 4  ==  15
--   circulo 5  ==  22
-- ----------------------------------------------------------------------------

circulo :: Int -> Int
circulo n = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 2. Definir por comprensión la función
--   aproxE :: Double -> [Double]
-- tal que '(aproxE n)' es la lista cuyos elementos son los términos de la
-- sucesión '(1+1/m)**m' desde 1 hasta 'n'. Por ejemplo,
--   aproxE 1 == [2.0]
--   aproxE 4 == [2.0,2.25,2.37037037037037,2.44140625]
-- ----------------------------------------------------------------------------

aproxE :: Double -> [Double]
aproxE n = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 3. Definir por comprensión la función
--   errorAproxE :: Double -> Double
-- tal que '(errorAproxE x)' es el menor número de términos de la sucesión
-- '(1+1/m)**m', necesarios para obtener su límite con un error menor que 'x'.
-- El límite de esta sucesión es el número e ≈ 2.718281828459045.
-- Por ejemplo,
--   errorAproxE 0.1    ==  13.0
--   errorAproxE 0.01   ==  135.0
--   errorAproxE 0.001  ==  1359.0
-- Indicación: En Haskell, e se calcula como (exp 1).
-- ----------------------------------------------------------------------------

errorAproxE :: Double -> Double
errorAproxE x = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 4. Definir por comprensión la función
--   aproxLimSeno :: Double -> [Double]
-- tal que '(aproxLimSeno n)' es la lista cuyos elementos son los términos de
-- la sucesión 'sen(1/m)/(1/m)' desde 1 hasta 'n'. Por ejemplo,
--   aproxLimSeno 1 == [0.8414709848078965]
--   aproxLimSeno 2 == [0.8414709848078965,0.958851077208406]
-- ----------------------------------------------------------------------------

aproxLimSeno :: Double -> [Double]
aproxLimSeno n = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 5. Definir por comprensión la función
--   errorLimSeno :: Double -> Double
-- tal que '(errorLimSeno x)' es el menor número de términos de la sucesión
-- 'sen(1/m)/(1/m)', necesarios para obtener su límite con un error menor que
-- 'x'. Por ejemplo,
--   errorLimSeno 0.1     ==  2.0
--   errorLimSeno 0.01    ==  5.0
--   errorLimSeno 0.001   ==  13.0
--   errorLimSeno 0.0001  ==  41.0
-- ----------------------------------------------------------------------------

errorLimSeno :: Double -> Double
errorLimSeno x = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 6. Definir por comprensión la función
--   calculaPi :: Double -> Double
-- tal que '(calculaPi n)' es la aproximación del número π calculada mediante
-- la expresión '4*(1 - 1/3 + 1/5 - 1/7 + ... + (-1)**n/(2*n+1))'. Por ejemplo,
--   calculaPi 3    ==  2.8952380952380956
--   calculaPi 300  ==  3.1449149035588526
-- ----------------------------------------------------------------------------

calculaPi :: Double -> Double
calculaPi n = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 7. Definir por comprensión la función
--   errorPi :: Double -> Double
-- tal que '(errorPi x)' es el menor número de términos de la serie
--   '4*(1 - 1/3 + 1/5 - 1/7 + ... + (-1)**n/(2*n+1))'
-- necesarios para obtener el número π con un error menor que 'x'. Por
-- ejemplo,
--   errorPi 0.1    ==  9.0
--   errorPi 0.01   ==  99.0
--   errorPi 0.001  ==  999.0
-- ----------------------------------------------------------------------------

errorPi :: Double -> Double
errorPi x = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 8. La suma de la serie
--   1/1^2 + 1/2^2 + 1/3^2 + 1/4^2 + ...
-- es π²/6. Por tanto, π se puede aproximar mediante la raíz cuadrada del
-- producto de 6 por dicha suma.
--
-- Definir por comprensión la función
--   aproximaPi :: Double -> Double
-- tal que '(aproximaPi n)' es la aproximación del número π obtenida mediante
-- 'n' términos de la serie anterior. Por ejemplo,
--   aproximaPi 4     ==  2.9226129861250305
--   aproximaPi 1000  ==  3.1406380562059946
-- ----------------------------------------------------------------------------

aproximaPi :: Double -> Double
aproximaPi n = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 9. Definir por comprensión la función
--   ocurrenciasDelMaximo :: [Int] -> (Int,Int)
-- tal que '(ocurrenciasDelMaximo xs)' es el par formado por el mayor de los
-- números de la lista 'xs' y el número de veces que éste aparece en 'xs', si
-- la lista es no vacía, y es (0,0) si 'xs' es la lista vacía. Por ejemplo,
--   ocurrenciasDelMaximo []                               ==  (0,0)
--   ocurrenciasDelMaximo [1,3,2,4,2,5,3,6,3,2,1,8,7,6,5]  ==  (8,1)
--   ocurrenciasDelMaximo [1,8,2,4,8,5,3,6,3,2,1,8]        ==  (8,3)
--   ocurrenciasDelMaximo [8,8,2,4,8,5,3,6,3,2,1,8]        ==  (8,4)
-- ----------------------------------------------------------------------------

ocurrenciasDelMaximo :: [Int] -> (Int,Int)
ocurrenciasDelMaximo xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 10. Definir por comprensión la función
--   tienenS :: [String] -> [Int]
-- tal que '(tienenS xss)' es la lista de las longitudes de las cadenas de
-- 'xss' que contienen el caracter 's' en mayúsculas o minúsculas. Por ejemplo,
--   tienenS ["Este","es","un","examen","de","hoy","Suerte"]  ==  [4,2,6]
--   tienenS ["Este"]                                         ==  [4]
--   tienenS []                                               ==  []
--   tienenS [" "]                                            ==  []
-- ----------------------------------------------------------------------------

tienenS :: [String] -> [Int]
tienenS xss = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 11. Una lista hermanada es una lista de números estrictamente
-- positivos en la que cada elemento tiene algún factor primo en común con el
-- siguiente, en caso de que exista, o alguno de los dos es un 1. Por ejemplo,
-- · [2,6,3,9,1,5] es una lista hermanada pues 2 y 6 tienen un factor en común
--   (2); 6 y 3 tienen un factor en común (3); 3 y 9 tienen un factor en común
--   (3); de 9 y 1 uno es el número 1; y de 1 y 5 uno es el número 1.
-- · [2,3,5] no es una lista hermanada pues 2 y 3 no tienen ningún factor primo
--   en común.
--
-- Definir por comprensión la función
--   hermanada :: [Int] -> Bool
-- tal que '(hermanada xs)' se verifica si la lista 'xs' es hermanada según la
-- definición anterior. Por ejemplo,
--   hermanada [2,6,3,9,1,5]  ==  True
--   hermanada [2,3,5]        ==  False
-- ----------------------------------------------------------------------------

hermanada :: [Int] -> Bool
hermanada xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 12. Definir por comprensión la función
--   tripletas :: [a] -> [[a]]
-- tal que '(tripletas xs)' es la lista de tripletas de elementos consecutivos
-- de la lista 'xs'. Por ejemplo,
--   tripletas [8,7,6,5,4]  ==  [[8,7,6],[7,6,5],[6,5,4]]
--   tripletas "abcd"       ==  ["abc","bcd"]
--   tripletas [2,4,3]      ==  [[2,4,3]]
--   tripletas [2,4]        ==  []
-- ----------------------------------------------------------------------------

tripletas :: [a] -> [[a]]
tripletas xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 13. Definir por comprensión la función
--   tresConsecutivas :: (Eq a) => a -> [a] -> Bool
-- tal que '(tresConsecutivas x ys)' se verifica si el elemento 'x' ocurre tres
-- veces seguidas en la lista 'ys'. Por ejemplo,
--   tresConsecutivas 3 [1,4,2,3,3,4,3,5,3,4,6]  ==  False
--   tresConsecutivas 'a' "abcaaadfg"            ==  True
-- ----------------------------------------------------------------------------

tresConsecutivas :: (Eq a) => a -> [a] -> Bool
tresConsecutivas x ys = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 14. Definir por comprensión la función
--   palabrasMasCortas :: [String] -> [String]
-- tal que '(palabrasMasCortas xs)' es la lista de las palabras más cortas (es
-- decir, de menor longitud) de la lista 'xs'. Por ejemplo,
--   palabrasMasCortas ["hoy","es","un","buen","dia","de","sol"]  ==
--     ["es","un","de"]
-- ----------------------------------------------------------------------------

palabrasMasCortas :: [String] -> [String]
palabrasMasCortas xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 15. Definir por comprensión la función
--   masOcurrentes :: (Eq a) => [a] -> [a]
-- tal que '(masOcurrentes xs)' es la lista de los elementos de 'xs' que
-- ocurren el máximo número de veces. Por ejemplo,
--   masOcurrentes [1,2,3,4,3,2,3,1,4]  ==  [3,3,3]
--   masOcurrentes [1,2,3,4,5,2,3,1,4]  ==  [1,2,3,4,2,3,1,4]
--   masOcurrentes "Salamanca"          ==  "aaaa"
-- ----------------------------------------------------------------------------

masOcurrentes :: (Eq a) => [a] -> [a]
masOcurrentes xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 16. Definir por comprensión la función
--   numeroDivisores :: Int -> Int
-- tal que '(numeroDivisores n)' es el número de divisores del número natural
-- 'n'. Por ejemplo,
--   numeroDivisores 11  ==  2
--   numeroDivisores 12  ==  6
-- ----------------------------------------------------------------------------

numeroDivisores :: Int -> Int
numeroDivisores n = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 17. Definir por comprensión la función
--   rangoConDivisores :: Int -> Int -> Int -> [Int]
-- tal que '(rangoConDivisores a b c)' es la lista de los números naturales
-- entre 'a' y 'b' con, al menos, 'c' divisores. Por ejemplo,
--   rangoConDivisores 11 16 5   ==  [12,16]
--   rangoConDivisores 1 100 10  ==  [48,60,72,80,84,90,96]
-- ----------------------------------------------------------------------------

rangoConDivisores :: Int -> Int -> Int -> [Int]
rangoConDivisores a b c = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 18. Definir por comprensión la función
--   enPosicionPar :: [a] -> [a]
-- tal que '(enPosicionPar cs)' es la cadena formada por los caracteres de 'cs'
-- que se encuentran en una posición par (comenzando desde 0). Por ejemplo,
--   enPosicionPar "el cielo sobre berlin"  ==  "e il or eln"
-- ----------------------------------------------------------------------------

enPosicionPar :: [a] -> [a]
enPosicionPar cs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 19. Un número n es de Angelini si n y 2n tienen algún dígito
-- común. Por ejemplo, 2014 es un número de Angelini ya que 2014 y su doble
-- (4028) comparten los dígitos 4 y 0.
--
-- Definir por comprensión la función
--   angelini :: Int -> Bool
-- tal que '(angelini n)' se verifica si 'n' es un número de Angelini. Por
-- ejemplo,
--   angelini 2014  ==  True
--   angelini 2067  ==  False
-- ----------------------------------------------------------------------------

angelini :: Int -> Bool
angelini n = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 20. En un bloque de pisos viven "Ana", "Beatriz", "Carlos" y
-- "Daniel", cada uno de ellos tiene ciertos alimentos en sus respectivas
-- despensas. Esta información está almacenada en una lista de asociación de la
-- siguiente forma: (<nombre>,<despensa>). Por ejemplo,
--   datos = [("Ana",["Leche","Huevos","Sal"]),
--            ("Beatriz",["Jamon","Lechuga"]),
--            ("Carlos",["Atun","Tomate","Jamon"]),
--            ("Daniel",["Salmon","Huevos"])]
--
-- Definir por comprensión la función
--   tienenProducto :: String -> [(String,[String])] -> [String]
-- tal que '(tienenProducto x ys)' es la lista de las personas que tienen el
-- producto 'x' en sus despensas. Por ejemplo,
--   tienenProducto "Lechuga" datos  ==  ["Beatriz"]
--   tienenProducto "Huevos"  datos  ==  ["Ana","Daniel"]
--   tienenProducto "Pan"     datos  ==  []
-- ----------------------------------------------------------------------------

datos :: [(String,[String])]
datos = [("Ana",["Leche","Huevos","Sal"]),
         ("Beatriz",["Jamon","Lechuga"]),
         ("Carlos",["Atun","Tomate","Jamon"]),
         ("Daniel",["Salmon","Huevos"])]

tienenProducto :: String -> [(String,[String])] -> [String]
tienenProducto x ys = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 21. Definir por comprensión la función
--   proveedores :: String -> [(String,[String])] -> [String]
-- tal que '(proveedores xs ys)' es la lista de las personas que pueden
-- proporcionar algún producto de los de la lista 'xs'. Por ejemplo,
--   proveedores ["Leche","Jamon"] datos  ==  ["Ana","Beatriz","Carlos"]
--   proveedores ["Sal","Atun"] datos     ==  ["Ana","Carlos"]
--   proveedores ["Leche","Sal"] datos    ==  ["Ana"]
-- ----------------------------------------------------------------------------

proveedores :: [String] -> [(String,[String])] -> [String]
proveedores xs ys = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 22. Sea t una lista de pares de la forma
--   '(nombre, [(asig_1, nota_1),...,(asig_k, nota_k)])'
-- Por ejemplo,
--   t1 = [("Ana",[("Alg",1),("Cal",3),("Inf",8),("Fis",2)]),
--         ("Juan",[("Alg",5),("Cal",1),("Inf",2),("Fis",9)]),
--         ("Alba",[("Alg",5),("Cal",6),("Inf",6),("Fis",5)]),
--         ("Pedro",[("Alg",9),("Cal",5),("Inf",3),("Fis",1)])]
--
-- Definir por comprensión la función
--   calificaciones :: [(String,[(String,Int)])] -> String -> [(String,Int)]
-- tal que '(calificaciones t p)' es la lista de las calificaciones del alumno
-- 'p' en la lista de notas 't'. Por ejemplo,
--   calificaciones t1 "Pedro"  ==  [("Alg",9),("Cal",5),("Inf",3),("Fis",1)]
-- ----------------------------------------------------------------------------

t1 :: [(String,[(String,Int)])]
t1 = [("Ana",[("Alg",1),("Cal",3),("Inf",8),("Fis",2)]),
      ("Juan",[("Alg",5),("Cal",1),("Inf",2),("Fis",9)]),
      ("Alba",[("Alg",5),("Cal",6),("Inf",6),("Fis",5)]),
      ("Pedro",[("Alg",9),("Cal",5),("Inf",3),("Fis",1)])]

calificaciones :: [(String,[(String,Int)])] -> String -> [(String,Int)]
calificaciones t p = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 23. Definir por comprensión la función
--   todasAprobadas :: [(String,[(String,Int)])] -> String -> Bool
-- tal que '(todasAprobadas t p)' se cumple si el alumno 'p' tiene todas las
-- asignaturas aprobadas en la lista de notas 't'. Por ejemplo,
--   todasAprobadas t1 "Alba"   ==  True
--   todasAprobadas t1 "Pedro"  ==  False
-- ----------------------------------------------------------------------------

todasAprobadas :: [(String,[(String,Int)])] -> String -> Bool
todasAprobadas t p = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 24. Definir por comprensión la función
--   aprobados :: [(String,[(String,Int)])] -> [String]
-- tal que '(aprobados t)' es la lista de los alumnos de la lista de notas 't'
-- que han aprobado todas las asignaturas. Por ejemplo,
--   aprobados t1  ==  ["Alba"]
-- ----------------------------------------------------------------------------

aprobados :: [(String,[(String,Int)])] -> [String]
aprobados t = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 25. Definir por comprensión la función
--   listaIgualParidad :: [Int] -> [Int]
-- tal que '(listaIgualParidad xs)' es la lista de todos los elementos de la
-- lista 'xs' con la misma paridad que la posición que ocupan (contada desde
-- 0); es decir, todos los pares en una posición par y todos los impares en una
-- posición impar. Por ejemplo,
--   listaIgualParidad [1,3,5,7]  ==  [3,7]
--   listaIgualParidad [2,4,6,8]  ==  [2,6]
--   listaIgualParidad [1..10]    ==  []
--   listaIgualParidad [0..10]    ==  [0,1,2,3,4,5,6,7,8,9,10]
--   listaIgualParidad []         ==  []
-- ----------------------------------------------------------------------------

listaIgualParidad :: [Int] -> [Int]
listaIgualParidad xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 26. Definir por comprensión la función
--   listaParesMenor :: (Ord a) => [a] -> [(a,a)]
-- tal que '(listaParesMenor xs)' es la lista de todos los pares de elementos
-- de la lista 'xs' tales que el primer elemento es estrictamente menor que el
-- segundo. Por ejemplo,
--   listaParesMenor [4,3,2,1]  ==  [(3,4),(2,4),(2,3),(1,4),(1,3),(1,2)]
--   listaParesMenor [1,1,3,4]  ==  [(1,3),(1,4),(1,3),(1,4),(3,4)]
--   listaParesMenor []         ==  []
--   listaParesMenor [1]        ==  []
-- ----------------------------------------------------------------------------

listaParesMenor :: (Ord a) => [a] -> [(a,a)]
listaParesMenor xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 27. Definir por comprensión la función
--   listaParesPosicionMenor :: [a] -> [(a,a)]
-- tal que '(listaParesPosicionMenor xs)' es la lista de todos los pares de
-- elementos de la lista 'xs' tales que el primer elemento ocupa una posición
-- estrictamente menor que el segundo. Por ejemplo,
--   listaParesPosicionMenor [4,3,2]  ==  [(4,3),(4,2),(3,2)]
--   listaParesPosicionMenor [1,1,3]  ==  [(1,1),(1,3),(1,3)]
--   listaParesPosicionMenor []       ==  []
--   listaParesPosicionMenor [1]      ==  []
-- ----------------------------------------------------------------------------

listaParesPosicionMenor :: [a] -> [(a,a)]
listaParesPosicionMenor xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 28. Definir por comprensión la función
--   numeroDigitosPares :: Integer -> Integer
-- tal que '(numeroDigitosPares n)' es la cantidad de dígitos pares que hay en
-- el número natural 'n'. Por ejemplo,
--   numeroDigitosPares 0       ==  1
--   numeroDigitosPares 1       ==  0
--   numeroDigitosPares 246     ==  3
--   numeroDigitosPares 135     ==  0
--   numeroDigitosPares 123456  ==  3
-- ----------------------------------------------------------------------------

numeroDigitosPares :: Integer -> Integer
numeroDigitosPares n = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 29. Definir por comprensión la función
--   listaMenor :: (Ord a) => [a] -> [a] -> Bool
-- tal que '(listaMenor xs ys)' se cumple si todos los elementos de la lista
-- 'xs' son estrictamente menores que todos los elementos de la lista 'ys'. Por
-- ejemplo,
--   listaMenor [1,2] [3,4]  ==  True
--   listaMenor [1,3] [2,4]  ==  False
--   listaMenor [] [2,4]     ==  True
--   listaMenor [1,2] []     ==  True
-- ----------------------------------------------------------------------------

listaMenor :: (Ord a) => [a] -> [a] -> Bool
listaMenor xs ys = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 30. Una secuencia creciente es aquella en la que todo elemento es
-- estrictamente menor que el siguiente. Una secuencia super-creciente es una
-- secuencia creciente en la que la secuencia de las diferencias entre
-- elementos consecutivos (un elemento menos el anterior) es creciente. Por
-- ejemplo, la secuencia [1,2,4,7] es creciente y la secuencia de las
-- diferencias entre elementos consecutivos es [1,2,3], que también es
-- creciente, por tanto la secuencia [1,2,4,7] es super-creciente. Por otro
-- lado, la secuencia [1,4,8,12] es creciente y la secuencia de las diferencias
-- entre elementos consecutivos es [3,4,4], que no es creciente, por tanto la
-- secuencia [1,4,8,12] no es super-creciente.
--
-- Definir por comprensión la función
--   superCreciente :: [Int] -> Bool
-- tal que '(superCreciente xs)' es verdad si y sólo si la secuencia 'xs' es
-- super-creciente. Por ejemplo,
--   superCreciente [1,2,4,7]   ==  True
--   superCreciente [1,4,8,12]  ==  False
-- ----------------------------------------------------------------------------

superCreciente :: [Int] -> Bool
superCreciente xs = undefined

-- ============================================================================
