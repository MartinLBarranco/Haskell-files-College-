-- I1M: Relacion_01.hs
-- Definiciones por composición
-- Departamento de Ciencias de la Computación e Inteligencia Artificial
-- Universidad de Sevilla
-- ============================================================================


-- ============================================================================
-- Definiciones por composición de funciones aritméticas
-- ============================================================================

-- ----------------------------------------------------------------------------
-- Ejercicio 1. Definir la función 'mediaA' tal que '(mediaA x y)' es la media
-- aritmética de los números 'x' e 'y'. Por ejemplo,
--   mediaA 2 3     ==  2.5
--   mediaA (-2) 5  ==  1.5
-- ----------------------------------------------------------------------------

mediaA x y = (x+y)/2

-- ----------------------------------------------------------------------------
-- Ejercicio 2. Definir la función 'mediaG' tal que '(mediaG x y)' es la media
-- geométrica de los números positivos 'x' e 'y'. Por ejemplo,
--   mediaG 2 3   ==  2.449489742783178
--   mediaG 2 18  ==  6.0
-- ----------------------------------------------------------------------------

mediaG x y = sqrt (x*y)

-- ----------------------------------------------------------------------------
-- Ejercicio 3. Definir la función 'mediaA3' tal que '(mediaA3 x y z)' es la
-- media aritmética de los números 'x', 'y' y 'z'. Por ejemplo,
--   mediaA3 1 3 8     ==  4.0
--   mediaA3 (-1) 0 7  ==  2.0
-- ----------------------------------------------------------------------------

mediaA3 x y z = (x+y+z)/3

-- ----------------------------------------------------------------------------
-- Ejercicio 4. Definir la función 'mediaG3' tal que '(mediaG3 x y z)' es la
-- media geométrica de los números positivos 'x', 'y' y 'z'. Por ejemplo,
--   mediaG3 1 3 9  ==  3.0
--   mediaG3 2 3 7  ==  3.4760266448864496
-- ----------------------------------------------------------------------------

mediaG3 x y z =  (x*y*z)**(1/3)

-- ----------------------------------------------------------------------------
-- Ejercicio 5. Definir la función 'longitudCircunferencia' tal que
-- '(longitudCircunferencia r)' es la longitud de la circunferencia de radio
-- 'r'. Por ejemplo,
--   longitudCircunferencia 1  ==  6.283185307179586
--   longitudCircunferencia 3  ==  18.84955592153876
-- Indicación: Usar la constante 'pi'.
-- ----------------------------------------------------------------------------

longitudCircunferencia r = 2*pi*r

-- ----------------------------------------------------------------------------
-- Ejercicio 6. Definir la función 'areaCirculo' tal que '(areaCirculo r)' es
-- el área del círculo de radio 'r'. Por ejemplo,
--   areaCirculo 1  ==  3.141592653589793
--   areaCirculo 3  ==  28.274333882308138
-- Indicación: Usar la constante 'pi'.
-- ----------------------------------------------------------------------------

areaCirculo r = (r^2)*pi

-- ----------------------------------------------------------------------------
-- Ejercicio 7. Definir la función 'areaCoronaCircular' tal que
-- '(areaCoronaCircular r1 r2)' es el área de la corona circular de radio
-- interior 'r1' y radio exterior 'r2'. Por ejemplo,
--   areaCoronaCircular 1 2  ==  9.42477796076938
--   areaCoronaCircular 3 5  ==  50.26548245743669
-- Indicación: Usar la constante 'pi'.
-- ----------------------------------------------------------------------------

areaCoronaCircular r1 r2 = (areaCirculo r1) - (areaCirculo r2)

-- ----------------------------------------------------------------------------
-- Ejercicio 8. Definir la función 'volumenEsfera' tal que '(volumenEsfera r)'
-- es el volumen de la esfera de radio 'r'. Por ejemplo,
--   volumenEsfera 1  ==  4.1887902047863905
--   volumenEsfera 3  ==  113.09733552923254
-- Indicación: Usar la constante 'pi'.
-- ----------------------------------------------------------------------------

volumenEsfera r = 0.75*pi*(r^3)

-- ----------------------------------------------------------------------------
-- Ejercicio 9. Definir la función 'areaEsfera' tal que '(areaEsfera r)' es el
-- área de la esfera de radio 'r'. Por ejemplo,
--   areaEsfera 1  ==  12.566370614359172
--   areaEsfera 3  ==  113.09733552923255
-- Indicación: Usar la constante 'pi'.
-- ----------------------------------------------------------------------------

areaEsfera r = 4*pi*(r^2)

-- ----------------------------------------------------------------------------
-- Ejercicio 10. Definir la función 'sumaEuros' tal que '(sumaEuros a b c d e)'
-- es la suma de los euros correspondientes a 'a' monedas de 1 euro, 'b' de 2
-- euros, 'c' billetes de 5 euros, 'd' de 10 euros y 'e' de 20 euros. Por
-- ejemplo,
--   sumaEuros 0 0 0 0 1  ==  20
--   sumaEuros 0 0 8 0 3  ==  100
--   sumaEuros 1 1 1 1 1  ==  38
-- ----------------------------------------------------------------------------

sumaEuros a b c d e = a + 2*b + 5*c + 10*d + 20*e

-- ----------------------------------------------------------------------------
-- Ejercicio 11. Definir la función 'ultimaCifra' tal que '(ultimaCifra x)' es
-- la última cifra del número 'x'. Por ejemplo,
--   ultimaCifra 325  ==  5
-- Indicación: Usar la función 'mod'.
-- ----------------------------------------------------------------------------

ultimaCifra x = x `mod` 10

-- ----------------------------------------------------------------------------
-- Ejercicio 12. Definir la función 'primeraCifra' tal que '(primeraCifra x)'
-- es la primera cifra del número 'x'. Por ejemplo,
--   primeraCifra 325  ==  3
-- Indicación: Usar la función 'log'.
-- ----------------------------------------------------------------------------

primeraCifra x = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 13. Definir la función 'maxTres' tal que '(maxTres x y z)' es el
-- máximo de los números 'x', 'y' y 'z'. Por ejemplo,
--   maxTres 6 2 4  ==  6
--   maxTres 6 7 4  ==  7
--   maxTres 6 7 9  ==  9
-- Indicación: Usar la función 'max'.
-- ----------------------------------------------------------------------------
maxDos x y |x>=y = x
           |otherwise = y

maxTres x y z = max x (max y z)

-- ----------------------------------------------------------------------------
-- Ejercicio 14. Definir la función 'raizEntera' tal que '(raizEntera n)' es el
-- mayor número entero 'm' tal que 'm^2' es menor que 'n'. Por ejemplo,
--   raizEntera 1     ==  1
--   raizEntera 12    ==  3
--   raizEntera 123   ==  11
--   raizEntera 1234  ==  35
-- ----------------------------------------------------------------------------

raizEntera n = floor(sqrt(n))

-- ----------------------------------------------------------------------------
-- Ejercicio 15. Definir la función 'ternaPitagorica' tal que
-- '(ternaPitagorica a b c)' comprueba si a²+b² = c². Por ejemplo,
--   ternaPitagorica 3 4 5  ==  True
--   ternaPitagorica 4 5 6  ==  False
-- ----------------------------------------------------------------------------

ternaPitagorica a b c | a^2 == b^2 + c^2 = True
                      | otherwise = False

-- ----------------------------------------------------------------------------
-- Ejercicio 16. Definir la función 'parabola' tal que '(parabola x)' es el
-- valor de la función x²+2x+1 en el punto 'x'. Por ejemplo,
--   parabola 2    ==  9.0
--   parabola 2.5  ==  12.25
--   parabola 3    ==  16.0
-- ----------------------------------------------------------------------------

parabola x = (x+1)^2

-- ----------------------------------------------------------------------------
-- Ejercicio 17. Definir la función 'bajoLaParabola' tal que
-- '(bajoLaParabola x y)' comprueba si el punto de coordenadas '(x,y)' está por
-- debajo de la parábola de ecuación x²+2x+1. Si el punto está en la parábola,
-- entonces no está por debajo de la parábola. Por ejemplo,
--   bajoLaParabola 0 0  ==  True
--   bajoLaParabola 1 1  ==  True
--   bajoLaParabola 1 5  ==  False
-- ----------------------------------------------------------------------------

bajoLaParabola x y | y < parabola x = True
                   | otherwise = False

-- ============================================================================
-- Definiciones por composición sobre listas
-- ============================================================================

-- ----------------------------------------------------------------------------
-- Ejercicio 18. Definir la función 'rotaI1' tal que '(rotaI1 xs)' es la lista
-- obtenida quitando el primer elemento de 'xs' y poniéndolo al final de la
-- lista. Por ejemplo,
--   rotaI1 [3,2,5,7]  ==  [2,5,7,3]
-- ----------------------------------------------------------------------------

rotaI1 (x:xs) = xs ++ [x]

-- ----------------------------------------------------------------------------
-- Ejercicio 19. Definir la función 'rotaI' tal que '(rotaI n xs)' es la lista
-- obtenida quitando los 'n' primeros elementos de 'xs' y poniéndolos en el
-- mismo orden al final de la lista. Por ejemplo,
--   rotaI 1 [3,2,5,7]  ==  [2,5,7,3]
--   rotaI 2 [3,2,5,7]  ==  [5,7,3,2]
--   rotaI 3 [3,2,5,7]  ==  [7,3,2,5]
-- ----------------------------------------------------------------------------

rotaI n xs = drop n xs ++ take n xs

-- ----------------------------------------------------------------------------
-- Ejercicio 20. Definir la función 'rotaD1' tal que '(rotaD1 xs)' es la lista
-- obtenida quitando el último elemento de 'xs' y poniéndolo al principio de la
-- lista. Por ejemplo,
--   rotaD1 [3,2,5,7]  ==  [7,3,2,5]
-- ----------------------------------------------------------------------------

rotaD1 xs = last xs : init xs

-- ----------------------------------------------------------------------------
-- Ejercicio 21. Definir la función 'rotaD' tal que '(rotaD n xs)' es la lista
-- obtenida quitando los 'n' últimos elementos de 'xs' y poniéndolos en el
-- mismo orden al principio de la lista. Por ejemplo,
--   rotaD 1 [3,2,5,7]  ==  [7,3,2,5]
--   rotaD 2 [3,2,5,7]  ==  [5,7,3,2]
--   rotaD 3 [3,2,5,7]  ==  [2,5,7,3]
-- ----------------------------------------------------------------------------

rotaD n xs = reverse(take n (reverse xs)) ++ take (length xs - n) xs

-- ----------------------------------------------------------------------------
-- Ejercicio 22. Definir la función 'rango' tal que '(rango xs)' es la lista
-- formada por el menor y el mayor elemento de 'xs'.
--   rango [3,2,7,5]  ==  [2,7]
-- Indicación: Usar 'minimum' y 'maximum'.
-- ----------------------------------------------------------------------------

rango xs = [minimum xs, maximum xs]

-- ----------------------------------------------------------------------------
-- Ejercicio 23. Definir la función 'palindromo' tal que '(palindromo xs)' se
-- verifica si 'xs' es un palíndromo; es decir, es lo mismo leer 'xs' de
-- izquierda a derecha que de derecha a izquierda. Por ejemplo,
--   palindromo [3,2,5,2,3]    ==  True
--   palindromo [3,2,5,6,2,3]  ==  False
-- ----------------------------------------------------------------------------

palindromo xs | xs == reverse xs = True
              | otherwise = False

-- ----------------------------------------------------------------------------
-- Ejercicio 24. Definir la función 'interior' tal que '(interior xs)' es la
-- lista obtenida eliminando los extremos de la lista. Por ejemplo,
--   interior [2,5,3,7,3]  ==  [5,3,7]
--   interior [2..7]       ==  [3,4,5,6]
-- ----------------------------------------------------------------------------

interior xs = tail(init xs)

-- ----------------------------------------------------------------------------
-- Ejercicio 25. Definir la función 'finales' tal que '(finales n xs)' es la
-- lista formada por los 'n' elementos finales de 'xs'. Por ejemplo,
--   finales 2 [2,5,4,7,9,6]  ==  [9,6]
--   finales 3 [2,5,4,7,9,6]  ==  [7,9,6]
--   finales 4 [2,5,4,7,9,6]  ==  [4,7,9,6]
-- ----------------------------------------------------------------------------

finales n xs = reverse(take (length xs - n) (reverse xs))

-- ----------------------------------------------------------------------------
-- Ejercicio 26. Definir la función 'segmento' tal que '(segmento n m xs)' es
-- la lista de los elementos de 'xs' comprendidos entre las posiciones 'n' y
-- 'm', ambas incluidas. Por ejemplo,
--   segmento 3 4 [3,4,1,2,7,9,0]  ==  [2,7]
--   segmento 3 5 [3,4,1,2,7,9,0]  ==  [2,7,9]
--   segmento 5 3 [3,4,1,2,7,9,0]  ==  []
-- ----------------------------------------------------------------------------

segmento n m xs = finales n (reverse(finales m (reverse xs)))

-- ----------------------------------------------------------------------------
-- Ejercicio 27. Definir la función 'extremos' tal que '(extremos n xs)' es la
-- lista formada por los 'n' primeros elementos de 'xs' y los 'n' elementos
-- finales de 'xs'. Por ejemplo,
--   extremos 1 [2,6,7,1,2,4,5,8,9,2,3]  ==  [2,3]
--   extremos 2 [2,6,7,1,2,4,5,8,9,2,3]  ==  [2,6,2,3]
--   extremos 3 [2,6,7,1,2,4,5,8,9,2,3]  ==  [2,6,7,9,2,3]
-- ----------------------------------------------------------------------------

extremos n xs = take n xs ++ reverse(take n (reverse xs))

-- ----------------------------------------------------------------------------
-- Ejercicio 28. Definir la función 'mitadInicial' tal que '(mitadInicial xs)'
-- es la lista formada por los elementos de la mitad inicial de 'xs'. Por
-- ejemplo,
--   mitadInicial [1,2,3,4,5]    ==  [1,2]
--   mitadInicial [1,2,3,4,5,6]  ==  [1,2,3]
-- ----------------------------------------------------------------------------

mitadInicial xs = take ((length xs) `div` 2) xs

-- ----------------------------------------------------------------------------
-- Ejercicio 29. Definir la función 'mediano' tal que '(mediano x y z)' es el
-- número mediano de entre los tres números 'x', 'y' y 'z'. Por ejemplo,
--   mediano 3 2 5  ==  3
--   mediano 2 4 5  ==  4
--   mediano 6 2 5  ==  5
--   mediano 2 6 6  ==  6
-- Indicación: Usar 'maximum' y 'minimum'.
-- ----------------------------------------------------------------------------

mediano x y z | maxTres x y z == x = max(y,z)
              | maxTres x y z == y = max(x,z)
              | otherwise = max(x,y)

-- ----------------------------------------------------------------------------
-- Ejercicio 30. Definir la función 'combinatorio' tal que '(combinatorio m n)'
-- es el valor del número combinatorio 'm' sobre 'n'. Por ejemplo,
--   combinatorio 10 1  ==  10
--   combinatorio 10 2  ==  45
--   combinatorio 10 3  ==  120
--   combinatorio 10 4  ==  210
--   combinatorio 10 5  ==  252
-- ----------------------------------------------------------------------------
fact m = product[1..m]

combinatorio m n = fact m `div` ((fact n)*fact (m-n))

-- ============================================================================
