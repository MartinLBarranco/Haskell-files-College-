-- I1M: Relaci�n 26
-- El TAD de los polinomios
-- Departamento de Ciencias de la Computaci�n e Inteligencia Artificial
-- Universidad de Sevilla
-- ============================================================================

-- ============================================================================
-- Introducci�n
-- ============================================================================

-- El objetivo de esta relaci�n es definir operaciones sobre polinomios
-- utilizando las implementaciones del TAD de polinomio. Para realizar los
-- ejercicios hay que descargar, en el mismo directorio que el enunciado, el
-- c�digo de los TAD
--   PolinomioConTipoDeDatoAlgebraico.hs
--   PolinomioConListaDensa.hs
--   PolinomioConListaDispersa.hs
--
-- El objetivo es hacer los ejercicios con una implementaci�n y comprobar que
-- las definiciones tambi�n son v�lidas con las otras.
--
-- Adem�s, en algunos ejemplos se usan polinomios con coeficientes racionales.
-- En Haskell, el n�mero racional x/y se representa por x%y. El TAD de los
-- n�meros racionales est� definido en el m�dulo Data.Ratio.

-- ============================================================================
-- Librer�as
-- ============================================================================

import Data.Ratio
import Test.QuickCheck
import I1M.Pol
-- Hay que elegir una implementaci�n del TAD polinomios.
--import PolinomioConTipoDeDatoAlgebraico
-- import PolinomioConListaDensa
-- import PolinomioConListaDispersa

-- ----------------------------------------------------------------------------
-- Ejercicio 1. Definir la funci�n
--   creaPolDispersa :: (Num a, Eq a) => [(Int,a)] -> Polinomio a
-- tal que '(creaPolDispersa xs)' es el polinomio cuya representaci�n dispersa
-- es la lista de pares 'xs'. Por ejemplo,
--   creaPolDispersa [(5,7),(2,4),(0,3)]  =>  7*x^5 + 4*x^2 + 3
-- ----------------------------------------------------------------------------

creaPolDispersa :: (Num a, Eq a) => [(Int,a)] -> Polinomio a
creaPolDispersa [] = polCero
creaPolDispersa (x:xs) = consPol (fst x) (snd x) (creaPolDispersa xs)

-- ----------------------------------------------------------------------------
-- Ejercicio 2. Definir la funci�n
--   creaPolDensa :: (Num a, Eq a) => [a] -> Polinomio a
-- tal que '(creaPolDensa xs)' es el polinomio cuya representaci�n densa es
-- la lista 'xs'. Por ejemplo,
--   creaPolDensa [7,0,0,4,0,3]  =>  7*x^5 + 4*x^2 + 3
-- ----------------------------------------------------------------------------

creaPolDensa :: (Num a, Eq a) => [a] -> Polinomio a
creaPolDensa xs = creaPolDispersa (zip [0..length xs] (reverse xs))

-- ----------------------------------------------------------------------------
-- Ejercicio 3. Definir la funci�n
--   dispersa :: (Num a, Eq a) => Polinomio a -> [(Int,a)]
-- tal que '(dispersa p)' es la representaci�n dispersa del polinomio 'p'. Por
-- ejemplo,
--   dispersa (creaPolDensa [1,0,0,5,4,0])  ==  [(5,1),(2,5),(1,4)]
-- ----------------------------------------------------------------------------

dispersa :: (Num a, Eq a) => Polinomio a -> [(Int,a)]
dispersa p | esPolCero p = []
           | otherwise = (grado p, coefLider p) : dispersa (consPol (grado p) (- coefLider p) p)

-- ----------------------------------------------------------------------------
-- Ejercicio 4. Definir la funci�n
--   densa :: (Num a, Eq a) => Polinomio a -> [a]
-- tal que '(densa p)' es la representaci�n densa del polinomio 'p'. Por
-- ejemplo,
--   densa (creaPolDensa [1,0,0,5,4,0])  ==  [1,0,0,5,4,0]
-- ----------------------------------------------------------------------------

densa :: (Num a, Eq a) => Polinomio a -> [a]
densa p = densaAux p (grado p)

densaAux p 0 = [coefLider p]
densaAux p n =
  if n == grado p
  then coefLider p : densaAux (restoPol p) (n-1)
  else 0 : densaAux p (n-1)

-- ----------------------------------------------------------------------------
-- Ejercicio 5. Definir la funci�n
--   coeficiente :: (Num a, Eq a) => Int -> Polinomio a -> a
-- tal que '(coeficiente k p)' es el coeficiente del t�rmino de grado 'k' del
-- polinomio 'p'. Por ejemplo,
--   coeficiente 2 (creaPolDensa [1,0,0,5,4,0])  ==  5
--   coeficiente 3 (creaPolDensa [1,0,0,5,4,0])  ==  0
-- ----------------------------------------------------------------------------

coeficiente :: (Num a, Eq a) => Int -> Polinomio a -> a
coeficiente k p |esPolCero p= error("No es posible")
                | k> grado p = error("Imposible de hallar")
                |grado p == k = coefLider p
                |otherwise = coeficiente k (consPol (grado p) (- coefLider p) p)

-- ----------------------------------------------------------------------------
-- Ejercicio 6. Definir la funci�n
--   creaTermino :: (Num a, Eq a) => Int -> a -> Polinomio a
-- tal que '(creaTermino n a)' es el t�rmino 'a*x^n'. Por ejemplo,
--   creaTermino 2 5  =>  5*x^2
-- ----------------------------------------------------------------------------

creaTermino :: (Num a, Eq a) => Int -> a -> Polinomio a
creaTermino n b = consPol n b polCero

-- ----------------------------------------------------------------------------
-- Ejercicio 7. Definir la funci�n
--   termLider :: (Num a, Eq a) => Polinomio a -> Polinomio a
-- tal que '(termLider p)' es el t�rmino l�der del polinomio 'p'. Por ejemplo,
--   termLider (creaPolDensa [1,0,0,5,4,0])  =>  x^5
-- ----------------------------------------------------------------------------

termLider :: (Num a, Eq a) => Polinomio a -> Polinomio a
termLider p = creaTermino (grado p) 1

-- ----------------------------------------------------------------------------
-- Ejercicio 8. Definir la funci�n
--   sumaPol :: (Num a, Eq a) => Polinomio a -> Polinomio a -> Polinomio a
-- tal que '(sumaPol p q)' es la suma de los polinomios 'p' y 'q'. Por ejemplo,
--   sumaPol (creaPolDensa [1,0,0,5,4,0]) (creaPolDensa [3,1])  =>
--     x^5 + 5*x^2 + 7*x + 1
-- ----------------------------------------------------------------------------
sumaPolAux :: (Num a, Eq a) => [a] -> [a] -> [a]
sumaPolAux xs ys | length xs == length ys = zipWith (+) xs ys
                 | length xs >length ys = take ((length xs)-(length ys)) xs ++sumaPolAux ((drop (length xs - length ys)) xs) ys
                 | otherwise = sumaPolAux ys xs


sumaPol :: (Num a, Eq a) => Polinomio a -> Polinomio a -> Polinomio a
sumaPol p q = creaPolDensa(sumaPolAux (densa p) (densa q) )

-- ----------------------------------------------------------------------------
-- Ejercicio 9. Definir la funci�n
--   multTermPol :: (Num a, Eq a) => Polinomio a -> Polinomio a -> Polinomio a
-- tal que '(multTermPol t p)' es el producto del t�rmino 't' por el polinomio
-- 'p'. Por ejemplo,
--   multTermPol (creaTermino 3 1) (creaPolDensa [3,1])  =>  3*x^4 + x^3
-- ----------------------------------------------------------------------------

multTermPol :: (Num a, Eq a) => Polinomio a -> Polinomio a -> Polinomio a
multTermPol q p = creaPolDispersa (map (\(x,y) -> (x+grado q, y*coefLider q)) (dispersa p))

-- ----------------------------------------------------------------------------
-- Ejercicio 10. Definir la funci�n
--   resta :: (Num a, Eq a) => Polinomio a -> Polinomio a -> Polinomio a
-- tal que '(resta p q)' es el polinomio obtenido restando el polinomio 'q' al
-- polinomio 'p'. Por ejemplo,
--   restaPol (creaPolDensa [1,0,0,5,4,0]) (creaPolDensa [3,1])  =>
--     x^5 + 5*x^2 + x - 1
-- ----------------------------------------------------------------------------
restaPolAux :: (Num a, Eq a) => [a] -> [a] -> [a]
restaPolAux xs ys | length xs == length ys = zipWith (-) xs ys
                  | length xs >length ys = take ((length xs)-(length ys)) xs ++sumaPolAux ((drop (length xs - length ys)) xs) ys
                  | otherwise = sumaPolAux ys xs



restaPol :: (Num a, Eq a) => Polinomio a -> Polinomio a -> Polinomio a
restaPol p q= creaPolDensa(restaPolAux (densa p) (densa q) )

-- ----------------------------------------------------------------------------
-- Ejercicio 11. Definir la funci�n
--   multPol :: (Num a, Eq a) => Polinomio a -> Polinomio a -> Polinomio a
-- tal que '(multPol p q)' es el producto de los polinomios 'p' y 'q'. Por
-- ejemplo,
--   multPol (creaPolDensa [1,0,0,5,4,0]) (creaPolDensa [3,1])  =>
--     3*x^6 + x^5 + 15*x^3 + 17*x^2 + 4*x
-- ----------------------------------------------------------------------------

multPol :: (Num a, Eq a) => Polinomio a -> Polinomio a -> Polinomio a
multPol p q |grado p == 0 || grado q == 0 = polCero
            |otherwise = sumaPol (multTermPol (creaTermino (grado p) (coefLider p)) q) (multPol (restoPol p) q)

-- ----------------------------------------------------------------------------
-- Ejercicio 12. Definir la funci�n
--   valor :: (Num a, Eq a) => Polinomio a -> a -> a
-- tal que '(valor p v)' es el valor del polinomio 'p' al sustituir su variable
-- por el n�mero 'c'. Por ejemplo,
--   valor (creaPolDensa [1,0,0,5,4,0]) 0     ==  0
--   valor (creaPolDensa [1,0,0,5,4,0]) 1     ==  10
--   valor (creaPolDensa [1,0,0,5,4,0]) (-2)  ==  -20
-- ----------------------------------------------------------------------------

valor :: (Num a, Eq a) => Polinomio a -> a -> a
valor p x | esPolCero p = 0
          | otherwise = (coefLider p) * (x^(grado p)) + valor (restoPol p) x

-- ----------------------------------------------------------------------------
-- Ejercicio 13. Definir la funci�n
--   potencia :: (Num a, Eq a) => Polinomio a -> Int -> Polinomio a
-- tal que '(potencia p n)' es la potencia 'n'-�sima del polinomio 'p'. Por
-- ejemplo,
--   potencia (creaPolDensa [3,1]) 2  =>  9*x^2 + 6*x + 1
--   potencia (creaPolDensa [3,1]) 3  =>  27*x^3 + 27*x^2 + 9*x + 1
-- ----------------------------------------------------------------------------

potencia :: (Num a, Eq a) => Polinomio a -> Int -> Polinomio a
potencia p 0 = creaTermino 0 1
potencia p n = multPol p (potencia p (n-1))

-- ----------------------------------------------------------------------------
-- Ejercicio 14. Definir la funci�n
--   potenciaM :: (Num a, Eq a) => Polinomio a -> Int -> Polinomio a
-- tal que '(potenciaM p n)' es la potencia 'n'-�sima del polinomio 'p', usando
-- las siguientes propiedades:
-- * Si n es par,   entonces x^n = (x^2)^(n/2)
-- * Si n es impar, entonces x^n = x * (x^2)^((n-1)/2)
-- Por ejemplo,
--   potenciaM (creaPolDensa [3,1]) 2  =>  9*x^2 + 6*x + 1
--   potenciaM (creaPolDensa [3,1]) 3  =>  27*x^3 + 27*x^2 + 9*x + 1
-- ----------------------------------------------------------------------------

potenciaM :: (Num a, Eq a) => Polinomio a -> Int -> Polinomio a
potenciaM = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 15. Definir la funci�n
--   derivada :: Polinomio Int -> Polinomio Int
-- tal que '(derivada p)' es la derivada del polinomio 'p' con respecto a su
-- variable. Por ejemplo,
--   derivada (creaPolDensa [1,0,0,5,4,0])  =>  5*x^4 + 10*x + 4
-- ----------------------------------------------------------------------------

derivada :: Polinomio Int -> Polinomio Int
derivada = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 16. Definir la funci�n
--   integral :: (Fractional a, Eq a) => Polinomio a -> Polinomio a
-- tal que '(integral p)' es la integral del polinomio 'p' con respecto a su
-- variable. En este caso los coeficientes del polinomio 'p' deben ser de la
-- clase 'Fractional', n�meros racionales o n�meros reales. Por ejemplo,
--   integral (creaPolDensa [3%1,0,5,0,3])  =>
--     3 % 5*x^5 + 5 % 3*x^3 + 3 % 1*x
--   integral (creaPolDensa [3.0,0,5,0,3])  =>
--     0.6*x^5 + 1.6666666666666667*x^3 + 3.0*x
-- ----------------------------------------------------------------------------

integral :: (Fractional a, Eq a) => Polinomio a -> Polinomio a
integral p | grado p == 0 = creaTermino (coefLider p) 0
           | otherwise = consPol (grado p +1) ((coefLider p)/(grado p +1)) (integral (resto p))

-- ----------------------------------------------------------------------------
-- Ejercicio 17. Definir la funci�n
--   integralDef :: (Fractional a, Eq a) => Polinomio a -> a -> a -> a
-- tal que '(integralDef p a b)' es la integral definida del polinomio 'p' con
-- respecto a su variable, en el intervalo '(a,b)'. En este caso los
-- coeficientes del polinomio 'p' deben ser de la clase 'Fractional', n�meros
-- racionales o n�meros reales. Por ejemplo,
--   integralDef (creaPolDensa [3%1,0,5,0,3]) 0 1  ==  79 % 15
--   integralDef (creaPolDensa [3.0,0,5,0,3]) 0 1  ==  5.266666666666667
-- ----------------------------------------------------------------------------

integralDef :: (Fractional a, Eq a) => Polinomio a -> a -> a -> a
integralDef = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 18. Definir la funci�n
--   multEscalar :: (Num a, Eq a) => a -> Polinomio a -> Polinomio a
-- tal que '(multEscalar c p)' es el polinomio obtenido multiplicando el n�mero
-- 'c' por el polinomio 'p'. Por ejemplo,
--   multEscalar 4 (creaPolDensa [3,1])  =>  12*x + 4
-- ----------------------------------------------------------------------------

multEscalar :: (Num a, Eq a) => a -> Polinomio a -> Polinomio a
multEscalar = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 19. Definir la funci�n
--   cociente :: (Fractional a, Eq a) =>
--               Polinomio a -> Polinomio a -> Polinomio a
-- tal que '(cociente p q)' es el cociente de la divisi�n del polinomio 'p'
-- entre el polinomio 'q'. En este caso los coeficientes de los polinomios 'p'
-- y 'q' deben ser de la clase 'Fractional', n�meros racionales o n�meros
-- reales. Por ejemplo,
--   cociente (creaPolDensa [3%1,0,5,0,3]) (creaPolDensa [6,2,0])  =>
--     1 % 2*x^2 - 1 % 6*x + 8 % 9
--   cociente (creaPolDensa [3.0,0,5,0,3]) (creaPolDensa [6,2,0])  =>
--     0.5*x^2 - 0.16666666666666666*x + 0.8888888888888888
-- ----------------------------------------------------------------------------

cociente :: (Fractional a, Eq a) => Polinomio a -> Polinomio a -> Polinomio a
cociente = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 20. Definir la funci�n
--   resto :: (Fractional a, Eq a) =>
--            Polinomio a -> Polinomio a -> Polinomio a
-- tal que '(resto p q)' es el resto de la divisi�n del polinomio 'p' entre el
-- polinomio 'q'. En este caso los coeficientes de los polinomios 'p' y 'q'
-- deben ser de la clase 'Fractional', n�meros racionales o n�meros reales. Por
-- ejemplo,
--   resto (creaPolDensa [3%1,0,5,0,3]) (creaPolDensa [6,2,0])  =>
--     - 16 % 9*x + 3 % 1
--   resto (creaPolDensa [3.0,0,5,0,3]) (creaPolDensa [6,2,0])  =>
--     - 1.7777777777777777*x + 3.0
-- ----------------------------------------------------------------------------

resto :: (Fractional a, Eq a) => Polinomio a -> Polinomio a -> Polinomio a
resto = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 21. Definir la funci�n
--   divisiblePol :: (Fractional a, Eq a) =>
--                   Polinomio a -> Polinomio a -> Bool
-- tal que '(divisiblePol p q)' se verifica si el polinomio 'p' es divisible
-- por el polinomio 'q'. Por ejemplo,
--   divisiblePol (creaPolDensa [6,2,0]) (creaPolDensa [3,1])  ==  True
--   divisiblePol (creaPolDensa [6,2,0]) (creaPolDensa [3,2])  ==  False
-- ----------------------------------------------------------------------------

divisiblePol :: (Fractional a, Eq a) => Polinomio a -> Polinomio a -> Bool
divisiblePol = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 22. El m�todo de Horner para calcular el valor de un polinomio se
-- basa en representarlo de una forma alternativa. Por ejemplo, para calcular
-- el valor de
--   a*x^5 + b*x^4 + c*x^3 + d*x^2 + e*x + f
-- este polinomio se representa como
--   ((((a * x + b) * x + c) * x + d) * x + e) * x + f
-- y se eval�a de dentro hacia afuera.
--
-- Definir la funci�n
--   horner :: (Eq a, Num a) => Polinomio a -> a -> a
-- tal que '(horner p c)' es el valor del polinomio 'p' al sustituir su
-- variable por el n�mero 'c', calculado usando el m�todo de Horner. Por
-- ejemplo,
--   horner (creaPolDensa [1,0,0,5,4,0]) 0      ==  0
--   horner (creaPolDensa [1,0,0,5,4,0]) 1      ==  10
--   horner (creaPolDensa [1,0,0,5,4,0]) 1.5    ==  24.84375
--   horner (creaPolDensa [1,0,0,5,4,0]) (3%2)  ==  795 % 32
-- ----------------------------------------------------------------------------

horner :: (Eq a, Num a) => Polinomio a -> a -> a
horner = undefined

-- ============================================================================
-- Factorizaci�n de polinomios
-- ============================================================================

-- ----------------------------------------------------------------------------
-- La regla de Ruffini facilita el c�lculo r�pido de la divisi�n de cualquier
-- polinomio entre un binomio de la forma (x-r). Adem�s, esta regla permite
-- localizar las ra�ces enteras de un polinomio y factorizarlo en binomios de
-- la forma (x-r) (siendo r un n�mero entero) siempre que sea posible.
--
-- La regla establece un m�todo para la divisi�n de un polinomio
--             p = an x^n + a(n-1) x^(n-1) + ... + a2 x^2 + a1 x + a0
-- entre un binomio de la forma (x-r). Para ello se trazan dos l�neas a modo de
-- ejes y se sit�an los coeficientes del polinomio ordenados y sin omitir
-- t�rminos nulos (es decir su representaci�n densa) en l�nea en la parte
-- superior; el n�mero r (del binomio (x-r)) se sit�a en el lado izquierdo del
-- eje vertical:
--
--     | an a(n-1) ... a2 a1 a0
--   r |
--  ---+------------------------
--     |
--
-- El primer coeficiente del polinomio se sit�a debajo del eje horizontal, en
-- la misma columna:
--
--     | an     a(n-1)    ... a2 a1 a0
--   r |
--  ---+--------------------------------
--     | an
--
-- A continuaci�n se multiplica r por an y se sit�a el resultado debajo del
-- siguiente t�rmino del polinomio (a(n-1)):
--
--     | an     a(n-1)    ... a2 a1 a0
--   r |         r*an
--  ---+--------------------------------
--     | an
--
-- Se suman los valores de esta columna y el resultado se coloca debajo del eje
-- horizontal, en la misma columna:
--
--     | an     a(n-1)    ... a2 a1 a0
--   r |         r*bn
--  ---+--------------------------------
--     | an  a(n-1)+r*an
--
-- A continuaci�n se repite el proceso de multiplicar r por el resultado de la
-- suma anterior; se coloca el resultado en la siguiente columna, debajo del
-- siguiente coeficiente del polinomio; se suman los n�meros de esta columna y
-- se coloca el resultado de la suma debajo del eje horizontal, en la misma
-- columna.
--
-- El proceso se repite hasta llegar a la �ltima columna, la del t�rmino
-- independiente del polinomio original.
--
-- El cociente de la divisi�n es el polinomio cuyos coeficientes (es decir, su
-- representaci�n densa) son los que est�n debajo del eje horizontal salvo el
-- �ltimo, y el resto de la divisi�n es el n�mero que aparece en la �ltima
-- columna debajo del eje horizontal.
--
-- Algunos ejemplos:
--
-- - Dividiendo el polinomio x^3 + 2 x^2 - x - 2 entre (x-2)
--
--         | 1  2  -1  -2
--       2 |    2   8  14
--     ----+--------------
--         | 1  4   7  12
--
--   Cociente: x^2 + 4 x + 7         Resto: 12
--
-- - Dividiendo el polinomio 2 x^3 + 3 x^2 - 4 entre (x+1)
--
--         | 2  3   0  -4
--      -1 |   -2  -1   1
--     ----+--------------
--         | 2  1  -1  -3
--
--   Cociente: 2 x^2 + x - 1         Resto: -3
--
-- Cuando el resto es igual a 0, conseguimos factorizar el polinomio original.
-- Por ejemplo, al dividir el polinomio x^3 + x^2 - x - 1 entre (x+1) se tiene
--
--         | 1  1  -1  -1
--      -1 |   -1   0   1
--     ----+--------------
--         | 1  0  -1   0
--
-- Por tanto el cociente es x^2 - 1 y el resto 0, lo que quiere decir que
-- x^3 + x^2 - x - 1 = (x+1)*(x^2 - 1)
--
-- Para que esto ocurra, el n�mero r (de (x-r)) tiene que ser un divisor del
-- t�rmino independiente del polinomio original.
--
-- Si se contin�a el proceso con el cociente de la divisi�n anterior y probando
-- con los valores r divisores de su t�rmino independiente, podemos llegar a
-- factorizar completamente el polinomio original en factores de la forma (x-r)
-- y, posiblemente, un �ltimo factor que es un polinomio sin raices enteras.
--
-- El objetivo de los siguientes ejercicios es implementar en Haskell este
-- proceso de factorizaci�n de polinomios.
-- ----------------------------------------------------------------------------

-- ----------------------------------------------------------------------------
-- Ejercicio 23. Definir la funci�n
--   reglaRuffini :: Int -> [Int] -> [Int]
-- tal que '(reglaRuffini r cs)' es la lista que resulta de aplicar la regla de
-- Ruffini al n�mero entero 'r' y a la lista de coeficientes 'cs'. Por ejemplo,
--   reglaRuffini 2 [1,2,-1,-2]  ==  [1,4,7,12]
--   reglaRuffini 1 [1,2,-1,-2]  ==  [1,3,2,0]
-- ya que
--      | 1  2  -1  -2           | 1  2  -1  -2
--    2 |    2   8  14         1 |    1   3   2
--    --+--------------        --+-------------
--      | 1  4   7  12           | 1  3   2   0
-- ----------------------------------------------------------------------------

reglaRuffini :: Int -> [Int] -> [Int]
reglaRuffini = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 24. Definir la funci�n
--   cocienteRuffini :: Int -> Polinomio Int -> Polinomio Int
-- tal que '(cocienteRuffini r p)' es el cociente de dividir el polinomio 'p'
-- por el polinomio 'x-r' usando la regla de Ruffini. Por ejemplo:
--   cocienteRuffini 2 (creaPolDensa [1,-3,3,-1])     =>  x^2 - x + 1
--   cocienteRuffini (-2) (creaPolDensa [1,-3,3,-1])  =>  x^2 - 5*x + 13
--   cocienteRuffini 3 (creaPolDensa [1,-3,3,-1])     =>  x^2 + 3
-- ----------------------------------------------------------------------------

cocienteRuffini :: Int -> Polinomio Int -> Polinomio Int
cocienteRuffini = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 25. Definir la funci�n
--   restoRuffini :: Int -> Polinomio Int -> Int
-- tal que '(restoRuffini r p)' es el resto de dividir el polinomio 'p' por el
-- polinomio 'x-r' usando la regla de Ruffini. Por ejemplo,
--   restoRuffini 2 (creaPolDensa [1,-3,3,-1])     ==  1
--   restoRuffini (-2) (creaPolDensa [1,-3,3,-1])  ==  -27
--   restoRuffini 3 (creaPolDensa [1,-3,3,-1])     ==  8
-- ----------------------------------------------------------------------------

restoRuffini :: Int -> Polinomio Int -> Int
restoRuffini = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 26. Comprobar con QuickCheck que, dado un polinomio 'p' no nulo y
-- un n�mero entero 'r', las funciones anteriores verifican la propiedad de la
-- divisi�n eucl�dea.
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_diviEuclidea :: Int -> Polinomio Int -> Property
prop_diviEuclidea = undefined

-- La comprobaci�n es
--   > quickCheck prop_diviEuclidea

-- ----------------------------------------------------------------------------
-- Ejercicio 27. Definir la funci�n
--   esRaizRuffini :: Int -> Polinomio Int -> Bool
-- tal que '(esRaizRuffini r p)' se verifica si 'r' es una ra�z de 'p', usando
-- para ello la regla de Ruffini. Por ejemplo,
--   esRaizRuffini 0 (creaPolDensa [1,-3,3,-1])  ==  False
--   esRaizRuffini 1 (creaPolDensa [1,-3,3,-1])  ==  True
-- ----------------------------------------------------------------------------

esRaizRuffini :: Int -> Polinomio Int -> Bool
esRaizRuffini = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 28. Definir la funci�n
--   divisores :: Int -> [Int]
-- tal que '(divisores n)' es la lista de todos los divisores enteros del
-- n�mero 'n'. Por ejemplo,
--   divisores 0  ==  [0]
--   divisores 1  ==  [1,-1]
--   divisores 4  ==  [4,-4,2,-2,1,-1]
-- ----------------------------------------------------------------------------

divisores :: Int -> [Int]
divisores = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 29. Definir la funci�n
--   raicesRuffini :: Polinomio Int -> [Int]
-- tal que '(raicesRuffini p)' es la lista de las raices enteras de 'p',
-- calculadas usando la regla de Ruffini. Por ejemplo,
--   raicesRuffini (creaPolDensa [1,0,0,5,4,0])  ==  [0,-1]
--   raicesRuffini (creaPolDensa [3,1])          ==  []
--   raicesRuffini (creaPolDensa [1,2,-1,-2])    ==  [-2,1,-1]
--   raicesRuffini (creaPolDensa [1,-3,3,-1])    ==  [1,1,1]
-- ----------------------------------------------------------------------------

raicesRuffini :: Polinomio Int -> [Int]
raicesRuffini = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 30. Definir la funci�n
--   factorizacion :: Polinomio Int -> [Polinomio Int]
-- tal que '(factorizacion p)' es la lista de la descomposici�n del polinomio
-- 'p' en factores obtenida mediante la regla de Ruffini. Por ejemplo,
--   factorizacion (creaPolDensa [1,0,0,5,4,0])  =>
--     [x^3 - x^2 + x + 4,x,x + 1]
--   factorizacion (creaPolDensa [3,1])          =>  [3*x + 1]
--   factorizacion (creaPolDensa [1,2,-1,-2])    =>  [x + 2,x - 1,x + 1]
--   factorizacion (creaPolDensa [1,-3,3,-1])    =>  [x - 1,x - 1,x - 1]
-- ----------------------------------------------------------------------------

factorizacion :: Polinomio Int -> [Polinomio Int]
factorizacion = undefined

-- ----------------------------------------------------------------------------
-- Generador de polinomios
-- ----------------------------------------------------------------------------

-- (genPol n) es un generador de polinomios. Por ejemplo,
--   ghci> sample (genPol 1)
--   0
--   x^9 + 4*x^5 - 2*x^2
--   - 5*x^10 + 4*x^9 + 3*x^7 - x^3 + 2
--   - 4*x^8 - 4*x^3 - 2*x
--   - 6*x^8 - 3
--   - 9*x^10 + 2*x^9 - 7*x^8 + 2*x^7 - 2*x^6 - 3*x^5 - 4*x^3 - 10
--   - 7
--   - 2*x^8 + 8*x^4 + x^2 + 12*x
--   5*x^5 - 15*x^2 - 8
--   11
--   16*x^9 + 9*x^7 + 8*x^5 - 14*x^4 - 19*x^2 + 5

genPol :: (Arbitrary a, Num a, Eq a, Ord a) => Int -> Gen (Polinomio a)
genPol 0 = return polCero
genPol n = do
  n <- choose (0,10)
  b <- arbitrary
  p <- genPol (div n 2)
  return (consPol n b p)

instance (Arbitrary a, Num a, Eq a, Ord a) => Arbitrary (Polinomio a) where
  arbitrary = sized genPol

-- ============================================================================
