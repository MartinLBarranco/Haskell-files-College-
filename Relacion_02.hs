-- I1M: Relacion_02.hs
-- Definiciones con condicionales, guardas y patrones
-- Departamento de Ciencias de la Computación e Inteligencia Artificial
-- Universidad de Sevilla
-- ============================================================================


-- ============================================================================
-- Librerías auxiliares
-- ============================================================================

import Test.QuickCheck

-- ============================================================================
-- La disyunción excluyente
-- ============================================================================

-- ----------------------------------------------------------------------------
-- La disyunción excluyente de dos fórmulas se verifica si una es verdadera
-- y la otra es falsa. La función asociada se llama 'xor' y su tabla de verdad
-- es:
--             X   |   Y   | xor X Y
--          -------+-------+---------
--           True  | True  |  False
--           True  | False |  True
--           False | True  |  True
--           False | False |  False
-- ----------------------------------------------------------------------------

-- ----------------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--   xor1 :: Bool -> Bool -> Bool
-- tal que '(xor1 x y)' es la disyunción excluyente de 'x' e 'y', calculada a
-- partir de la tabla de verdad. Usar 4 ecuaciones, una por cada línea de la
-- tabla.
-- ----------------------------------------------------------------------------

xor1 :: Bool -> Bool -> Bool
xor1 True True = False
xor1 False False = False
xor1 True False = True
xor1 False True = True

-- ----------------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--   xor2 :: Bool -> Bool -> Bool
-- tal que '(xor2 x y)' es la disyunción excluyente de 'x' e 'y', calculada a
-- partir de la tabla de verdad y patrones. Usar 2 ecuaciones, una por cada
-- valor del primer argumento.
-- ----------------------------------------------------------------------------

xor2 :: Bool -> Bool -> Bool
xor2 x y | x == y = False
         | otherwise = True

-- ----------------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--   xor3 :: Bool -> Bool -> Bool
-- tal que '(xor3 x y)' es la disyunción excluyente de 'x' e 'y', calculada a
-- partir de la disyunción (||), la conjunción (&&) y la negación (not), usando
-- una única ecuación.
-- ----------------------------------------------------------------------------

xor3 :: Bool -> Bool -> Bool
xor3 x y = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--   xor4 :: Bool -> Bool -> Bool
-- tal que '(xor4 x y)' es la disyunción excluyente de 'x' e 'y', calculada a
-- partir de desigualdad (/=), usando una única ecuación.
-- ----------------------------------------------------------------------------

xor4 :: Bool -> Bool -> Bool
xor4 x y | x /= y = True
         | otherwise = False

-- ----------------------------------------------------------------------------
-- Ejercicio 5. Comprobar con QuickCheck que las cuatro definiciones anteriores
-- de la disyunción excluyente son equivalentes.
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_xor_equivalentes :: Bool -> Bool -> Bool
prop_xor_equivalentes x y = undefined

-- La comprobación es
--   > quickCheck prop_xor_equivalentes

-- ============================================================================
-- Definiciones con condicionales y guardas
-- ============================================================================

-- ----------------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--   divisionSegura :: Double -> Double -> Double
-- tal que '(divisionSegura x y)' es 'x/y' si 'y' no es cero y 9999 en caso
-- contrario. Por ejemplo,
--   divisionSegura 7 2  ==  3.5
--   divisionSegura 7 0  ==  9999.0
-- Realizar tres definiciones:
-- *) divisionSeguraEq: Usando ecuaciones
-- *) divisionSeguraIf: Usando condicionales
-- *) divisionSeguraGr: Usando guardas
-- ----------------------------------------------------------------------------

divisionSeguraEq :: Double -> Double -> Double
divisionSeguraEq x y = undefined

divisionSeguraIf :: Double -> Double -> Double
divisionSeguraIf x y = if y == 0 then 9999.0 else x/y

divisionSeguraGr :: Double -> Double -> Double
divisionSeguraGr x y | y == 0 = 9999.0
                     | otherwise = x/y

-- ----------------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--   mayorNumero :: Int -> Int -> Int
-- tal que '(mayorNumero x y)' es el mayor número de dos cifras que se puede
-- construir con los dígitos 'x' e 'y'. Por ejemplo,
--   mayorNumero 2 5 ==  52
--   mayorNumero 5 2 ==  52
-- Realizar dos definiciones:
-- *) mayorNumeroIf: Usando condicionales
-- *) mayorNumeroGr: Usando guardas
-- ----------------------------------------------------------------------------

mayorNumeroGr :: Int -> Int -> Int
mayorNumeroGr x y | x >= y = x*10+y
                  | otherwise = y*10+x

mayorNumeroIf :: Int -> Int -> Int
mayorNumeroIf x y = if x>=y then x*10+y else y*10+x

-- ----------------------------------------------------------------------------
-- Ejercicio 8. Definir la función
--   numeroDeRaices :: Double -> Double -> Double -> Int
-- tal que '(numeroDeRaices a b c)' es el número de raíces reales de la
-- ecuación a*x² + b*x + c = 0. Por ejemplo,
--   numeroDeRaices 2 0 3    ==  0
--   numeroDeRaices 4 4 1    ==  1
--   numeroDeRaices 5 23 12  ==  2
-- Realizar dos definiciones:
-- *) numeroDeRaicesIf: Usando condicionales
-- *) numeroDeRaicesGr: Usando guardas
-- ----------------------------------------------------------------------------

numeroDeRaicesIf :: Double -> Double -> Double -> Int
numeroDeRaicesIf a b c = if f > 0 then 2 else
                           if f == 0 then 1 else 0
                              where f = sqrt((b^2)-(4*a*c))

numeroDeRaicesGr :: Double -> Double -> Double -> Int
numeroDeRaicesGr a b c | sqrt(b^2-4*a*c) > 0 = 2
                       | sqrt(b^2-4*a*c) == 0 = 1
                       | otherwise = 0

-- ----------------------------------------------------------------------------
-- Ejercicio 9. Definir la función
--   raicesEcuacion2Grado :: Double -> Double -> Double -> [Double]
-- de forma que '(raicesEcuacion2Grado a b c)' devuelve la lista de las raices
-- reales de la ecuación a*x² + b*x + c = 0. Si la ecuación tiene una única
-- raíz real, se devuelve una lista con dicha raíz duplicada (raíz doble). Por
-- ejemplo,
--   raicesEcuacion2Grado 1 3 2     ==  [-1.0,-2.0]
--   raicesEcuacion2Grado 1 (-2) 1  ==  [1.0,1.0]
--   raicesEcuacion2Grado 1 0 1     ==  []
-- Realizar dos definiciones:
-- *) raicesEcuacion2GradoIf: Usando condicionales
-- *) raicesEcuacion2GradoGr: Usando guardas
-- ----------------------------------------------------------------------------



raicesEcuacion2GradoIf :: Double -> Double -> Double -> [Double]
raicesEcuacion2GradoIf a b c = if numeroDeRaicesIf a b c == 0 then [] else
                                 if numeroDeRaicesIf a b c == 1 then [-b/(2*a)] else [(-b + sqrt(b^2-4*a*c))/(2*a), (-b - sqrt(b^2-4*a*c))/(2*a)]

raicesEcuacion2GradoGr :: Double -> Double -> Double -> [Double]
raicesEcuacion2GradoGr a b c | numeroDeRaicesIf a b c == 0 = []
                             | numeroDeRaicesIf a b c == 1 = [-b/(2*a)]
                             | otherwise = [(-b + sqrt(b^2-4*a*c))/(2*a), (-b - sqrt(b^2-4*a*c))/(2*a)]
-- ----------------------------------------------------------------------------
-- Ejercicio 10. Definir el operador
--   (~=) :: Double -> Double -> Bool
-- tal que '(x ~= y)' se verifica si 'x' e 'y' son casi iguales; es decir si el
-- valor absoluto de su diferencia es menor que una milésima. Por ejemplo,
--   12.3457 ~= 12.3459  ==  True
--   12.3457 ~= 12.3479  ==  False
-- ----------------------------------------------------------------------------

(~=) :: Double -> Double -> Bool
x ~= y = abs (x-y) < 0.001 

-- ----------------------------------------------------------------------------
-- Ejercicio 11. Comprobar con QuickCheck que la suma de las raíces de la
-- ecuación a*x² + b*x + c = 0 (con a no nulo) es casi igual que -b/a y su
-- producto es casi igual que c/a.
--
-- Nota. En la comparación usar ~= en lugar de ==
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_raices :: Double -> Double -> Double -> Property
prop_raices a b c = undefined

-- La comprobación es
--   > quickCheck prop_raices

-- ============================================================================
-- Números Racionales
-- ============================================================================

-- ----------------------------------------------------------------------------
-- Los números racionales se pueden representar mediante pares de números
-- enteros. Por ejemplo, el número 2/5 se puede representar mediante el par
-- (2,5).
-- ----------------------------------------------------------------------------

-- ----------------------------------------------------------------------------
-- Ejercicio 12. Definir la función
--   formaReducida :: (Integer,Integer) -> (Integer,Integer)
-- tal que '(formaReducida x)' es la forma reducida del número racional 'x'.
-- Por ejemplo,
--   formaReducida (4,10)  ==  (2,5)
-- ----------------------------------------------------------------------------

formaReducida :: (Integer,Integer) -> (Integer,Integer)
formaReducida (x1,x2) | gcd x1 x2 == 1 = (x1, x2)
                      | otherwise = formaReducida (x1 `div` a, x2 `div` a)
                        where a = gcd x1 x2

-- ----------------------------------------------------------------------------
-- Ejercicio 13. Definir la función
--   sumaRacional :: (Integer,Integer) ->
--                   (Integer,Integer) -> (Integer,Integer)
-- tal que '(sumaRacional x y)' es la suma de los números racionales 'x' e 'y'.
-- Por ejemplo,
--   sumaRacional (2,3) (5,6)  ==  (3,2)
-- ----------------------------------------------------------------------------

sumaRacional :: (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer)
sumaRacional (x1,x2) (y1,y2) = formaReducida (x1*y2 + x2*y1, x2*y2)

-- ----------------------------------------------------------------------------
-- Ejercicio 14. Definir la función
--   productoRacional :: (Integer,Integer) ->
--                       (Integer,Integer) -> (Integer,Integer)
-- tal que '(productoRacional x y)' es el producto de los números racionales
-- 'x' e 'y'. Por ejemplo,
--   productoRacional (2,3) (5,6)  ==  (5,9)
-- ----------------------------------------------------------------------------

productoRacional :: (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer)
productoRacional (x1,x2) (y1,y2) = formaReducida (x1*y1,x2*y2)

-- ----------------------------------------------------------------------------
-- Ejercicio 15. Definir la función
--   cocienteRacional :: (Integer,Integer) ->
--                       (Integer,Integer) -> (Integer,Integer)
-- tal que '(cocienteRacional x y)' es el cociente de los números racionales
-- 'x' e 'y'. Por ejemplo,
--   cocienteRacional (2,3) (5,6)  ==  (4,5)
-- ----------------------------------------------------------------------------

cocienteRacional :: (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer)
cocienteRacional (x1,x2) (y1,y2) = productoRacional (x1,x2) (y2,y2)

-- ----------------------------------------------------------------------------
-- Ejercicio 16. Definir la función
--   igualdadRacional :: (Integer,Integer) -> (Integer,Integer) -> Bool
-- tal que '(igualdadRacional x y)' se verifica si los números racionales 'x' e
-- 'y' son iguales. Por ejemplo,
--   igualdadRacional (6,9) (10,15)  ==  True
--   igualdadRacional (6,9) (11,15)  ==  False
-- ----------------------------------------------------------------------------

igualdadRacional :: (Integer,Integer) -> (Integer,Integer) -> Bool
igualdadRacional (x1,x2) (y1,y2) = (x1,x2) == formaReducida (y1,y2)

-- ----------------------------------------------------------------------------
-- Ejercicio 17. Comprobar con QuickCheck la propiedad distributiva del
-- producto de números racionales respecto de la suma de números racionales.
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_distributiva :: (Integer,Integer) ->
                     (Integer,Integer) -> (Integer,Integer) -> Property
prop_distributiva x y z = undefined

-- La comprobación es
--   > quickCheck prop_distributiva

-- ============================================================================
-- Ejercicios relacionados con el plano real
-- ============================================================================

-- ----------------------------------------------------------------------------
-- Ejercicio 18. Los rectángulos pueden representarse por sus dimensiones, base
-- y altura, como un par de números enteros. Por ejemplo, (5,3) representa un
-- rectángulo de base 5 y altura 3.
--
-- Definir la función
--   mayorRectangulo :: (Int,Int) -> (Int,Int) -> (Int,Int)
-- tal que '(mayorRectangulo r1 r2)' es el rectángulo de mayor área entre 'r1'
-- y 'r2'. Por ejemplo,
--   mayorRectangulo (4,6) (3,7)  ==  (4,6)
--   mayorRectangulo (4,6) (3,8)  ==  (4,6)
--   mayorRectangulo (4,6) (3,9)  ==  (3,9)
-- ----------------------------------------------------------------------------

mayorRectangulo :: (Int,Int) -> (Int,Int) -> (Int,Int)
mayorRectangulo (x1,y1) (x2,y2) = if x1*y1 >= x2*y2 then (x1,y1) else (x2,y2)

-- ----------------------------------------------------------------------------
-- Ejercicio 19. Definir la función
--   cuadrante :: (Int,Int) -> Int
-- tal que '(cuadrante p)' es el cuadrante en el que se encuentra el punto 'p'.
-- Si el punto está sobre los ejes el resultado debe ser 0. Por ejemplo,
--   cuadrante (0,4)    ==  0
--   cuadrante (-3,0)   ==  0
--   cuadrante (0,0)    ==  0
--   cuadrante (3,5)    ==  1
--   cuadrante (-3,5)   ==  2
--   cuadrante (-3,-5)  ==  3
--   cuadrante (3,-5)   ==  4
-- ----------------------------------------------------------------------------

cuadrante :: (Int,Int) -> Int
cuadrante (x1,x2) | x1 == 0 || x2 == 0 = 0
                  | signum x1 == 1 && signum x2 == 1 = 1
                  | signum x1 == -1 && signum x2 == -1 = 3
                  | signum x1 == 1 && signum x2 == -1 = 4
                  | otherwise = 2

-- ----------------------------------------------------------------------------
-- Ejercicio 20. Definir la función
--   simetricoH :: (Int,Int) -> (Int,Int)
-- tal que '(simetricoH p)' es el punto simétrico de 'p' respecto del eje
-- horizontal. Por ejemplo,
--   simetricoH (2,5)   ==  (2,-5)
--   simetricoH (2,-5)  ==  (2,5)
-- ----------------------------------------------------------------------------

simetricoH :: (Int,Int) -> (Int,Int)
simetricoH (x1,x2) = (x1,-x2)

-- ----------------------------------------------------------------------------
-- Ejercicio 21. Definir la función
--   simetricoV :: (Int,Int) -> (Int,Int)
-- tal que '(simetricoV p)' es el punto simétrico de 'p' respecto del eje
-- vertical. Por ejemplo,
--   simetricoV (2,5)   ==  (-2,5)
--   simetricoV (2,-5)  ==  (-2,-5)
-- ----------------------------------------------------------------------------

simetricoV :: (Int,Int) -> (Int,Int)
simetricoV (x1,x2) = (-x1,x2)

-- ----------------------------------------------------------------------------
-- Ejercicio 22. Definir la función
--   distancia :: (Float,Float) -> (Float,Float) -> Float
-- tal que '(distancia p1 p2)' es la distancia entre los puntos 'p1' y 'p2'.
-- Por ejemplo,
--   distancia (1,2) (4,6)  ==  5.0
-- ----------------------------------------------------------------------------

distancia :: (Float,Float) -> (Float,Float) -> Float
distancia (x1,x2) (y1,y2) = sqrt((y1-x1)^2+(y2-x2)^2)

-- ----------------------------------------------------------------------------
-- Ejercicio 23. Comprobar con QuickCheck que se verifica la propiedad
-- triangular de la distancia; es decir, dados tres puntos p1, p2 y p3, la
-- distancia de p1 a p3 es menor o igual que la suma de las distancias de p1 a
-- p2 y de p2 a p3.
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_triangular :: (Float,Float) -> (Float,Float) -> (Float,Float) -> Bool
prop_triangular p1 p2 p3 = undefined

-- La comprobación es
--   > quickCheck prop_triangular

-- ----------------------------------------------------------------------------
-- Ejercicio 24. Definir la función
--   puntoMedio :: (Float,Float) -> (Float,Float) -> (Float,Float)
-- tal que '(puntoMedio p1 p2)' es el punto medio del segmento cuyos extremos
-- son los puntos 'p1' y 'p2'. Por ejemplo,
--   puntoMedio (0,2) (0,6)   ==  (0.0,4.0)
--   puntoMedio (-1,2) (7,6)  ==  (3.0,4.0)
-- ----------------------------------------------------------------------------

puntoMedio :: (Float,Float) -> (Float,Float) -> (Float,Float)
puntoMedio (x1,x2) (y1,y2) = (x1+(y1-x1)/2,x2+(y2-x2)/2)

-- ----------------------------------------------------------------------------
-- Ejercicio 25. Definir la función
--   alineados :: (Float,Float) -> (Float,Float) -> (Float,Float) -> Bool
-- tal que '(alineados p1 p2 p3)' se verifica si los tres puntos 'p1', 'p2' y
-- 'p3' son distintos y están alineados. Por ejemplo,
--   alineados (0,0) (3,0) (6,0)   ==  True
--   alineados (0,1) (0,-1) (0,3)  ==  True
--   alineados (1,3) (2,6) (3,9)   ==  True
--   alineados (2,5) (1,2) (-1,3)  ==  False
-- ----------------------------------------------------------------------------

alineados :: (Float,Float) -> (Float,Float) -> (Float,Float) -> Bool
alineados (x1,x2) (y1,y2) (z1,z2) = (y2-x2)/(y1-x1) == (z2-y2)/(z1-y1)

-- ----------------------------------------------------------------------------
-- Ejercicio 26. Definir la función
--   anguloRecto :: (Float,Float) -> (Float,Float) -> (Float,Float) -> Bool
-- tal que '(anguloRecto p1 p2 p3)' se verifica si los puntos 'p1', 'p2' y 'p3'
-- son los vértices de un triángulo rectángulo con el ángulo recto en el
-- vértice 'p1'. Por ejemplo,
--   anguloRecto (1,1) (6,6) (2,0)  ==  True
--   anguloRecto (1,1) (5,0) (2,5)  ==  True
--   anguloRecto (1,1) (2,1) (2,3)  ==  False
--   anguloRecto (2,0) (0,2) (3,3)  ==  False
-- ----------------------------------------------------------------------------

anguloRecto :: (Float,Float) -> (Float,Float) -> (Float,Float) -> Bool
anguloRecto (x1,y1) (x2,y2) (x3,y3) = (x2-x1)*(x3-x1)+(y2-y1)*(y3-y1) == 0

-- ----------------------------------------------------------------------------
-- Ejercicio 27. Definir la función
--   perpendiculares :: (Float,Float) -> ((Float,Float),(Float,Float))
-- tal que '(perpendiculares p)' es el par de puntos del plano 'q1' y 'q2' que
-- están a la misma distancia del origen 'O' (0,0) que el punto 'p' y tales que
-- el ángulo pOq es un ángulo recto. Por ejemplo,
--   perpendiculares (2,1)  ==  ((1.0,-2.0),(-1.0,2.0))
--   perpendiculares (0,2)  ==  ((2.0,0.0),(-2.0,0.0))
-- ----------------------------------------------------------------------------

perpendiculares :: (Float,Float) -> ((Float,Float),(Float,Float))
perpendiculares (x,y) = ((y,-x),(-y,x))

-- ----------------------------------------------------------------------------
-- Ejercicio 28. Comprobar con QuickCheck que cualquiera de los dos puntos
-- obtenidos con la función 'perpendiculares', junto con el origen 'O' (0,0) y
-- el punto original, son los vértices de un triángulo rectángulo con el ángulo
-- recto en el punto 'O'. Usar la función 'anguloRecto' definida anteriormente.
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_perpendiculares :: (Float,Float) -> Bool
prop_perpendiculares p = undefined

-- La comprobación es
--   > quickCheck prop_perpendiculares

-- ----------------------------------------------------------------------------
-- Ejercicio 29. En geometría, la fórmula de Herón, descubierta por Herón de
-- Alejandría, dice que el área de un triángulo cuyo lados miden a, b y c es la
-- raíz cuadrada de s(s-a)(s-b)(s-c) donde s es el semiperímetro (a+b+c)/2.
--
-- Definir la función
--   areaTrianguloHeron :: Float -> Float -> Float -> Float
-- tal que '(areaTrianguloHeron a b c)' es el área de un triángulo de lados
-- 'a', 'b' y 'c', calculada con la fórmula de Herón. Por ejemplo,
--   areaTrianguloHeron 3 4 5  ==  6.0
-- ----------------------------------------------------------------------------

areaTrianguloHeron :: Float -> Float -> Float -> Float
areaTrianguloHeron a b c = sqrt(s*(s-a)*(s-b)*(s-c))
                             where s = (a+b+c)/2

-- ----------------------------------------------------------------------------
-- Ejercicio 30. Los intervalos cerrados se pueden representar mediante una
-- lista de dos números (el primero es el extremo inferior del intervalo y el
-- segundo el superior).
--
-- Definir la función
--   interseccion :: [Int] -> [Int] -> [Int]
-- tal que '(interseccion i1 i2)' es la intersección de los intervalos 'i1' e
-- 'i2'. Por ejemplo,
--   interseccion [] [3,5]     ==  []
--   interseccion [3,5] []     ==  []
--   interseccion [2,4] [6,9]  ==  []
--   interseccion [2,6] [6,9]  ==  [6,6]
--   interseccion [2,6] [0,9]  ==  [2,6]
--   interseccion [2,6] [0,4]  ==  [2,4]
--   interseccion [4,6] [0,4]  ==  [4,4]
--   interseccion [5,6] [0,4]  ==  []
-- ----------------------------------------------------------------------------

interseccion :: [Int] -> [Int] -> [Int]
interseccion [] _ = []
interseccion _ [] = []
interseccion i1 i2 = undefined

-- ============================================================================
