-- I1M: Relación 18
-- Codificación César con la librería Data.Char
-- Departamento de Ciencias de la Computación e Inteligencia Artificial
-- Universidad de Sevilla
-- ============================================================================

-- ============================================================================
-- Librerías auxiliares
-- ============================================================================
import Data.List (nub)
import Data.Char

-- ============================================================================
-- Codificación César
-- ============================================================================

-- El cifrado César, también conocido como cifrado por desplazamiento, código
-- de César o desplazamiento de César, es una de las técnicas de cifrado más
-- simples y más usadas. Es un tipo de cifrado por sustitución en el que una
-- letra en el texto original es reemplazada por otra letra que se encuentra un
-- número fijo de posiciones más adelante en el alfabeto. Por ejemplo, con un
-- desplazamiento de 3, la A sería sustituida por la D (situada 3 lugares a la
-- derecha de la A), la B sería reemplazada por la E, etc. Este método debe su
-- nombre a Julio César, que lo usaba para comunicarse con sus generales.

-- De esta forma, para codificar un mensaje con un desplazamiento dado, hay que
-- reemplazar todas las letras del mensaje original. Por ejemplo, la
-- codificación del mensaje "Esto es un ejemplo" con un desplazamiento de 3
-- posiciones, resulta en el mensaje "Hvwr hv xq hmhpsor". Como se puede
-- observar se respetan las mayúsculas del mensaje original. El proceso de
-- cifrado es reversible, es decir, si se aplica el proceso de codificación con
-- un desplazamiento de (-3) al mensaje cifrado, "Hvwr hv xq hmhpsor", se
-- obtendrá el mensaje original, "Esto es un ejemplo".

-- Para simplificar el desarrollo de la codificación César, no se tendrán en
-- cuenta las letras ñ y Ñ, ni las vocales acentuadas. Es decir, el codificador
-- que vamos a construir no se podrá usar en textos que contengan las letras ñ,
-- Ñ ni las vocales acentuadas.

-- ----------------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--   letra2int :: Char -> Int
-- tal que '(letra2int c)' es el entero correspondiente a la letra 'c', que
-- puede estar tanto en mayúscula como en minúscula. Por ejemplo,
--   letra2int 'a'  ==  0
--   letra2int 'D'  ==  3
--   letra2int 'z'  ==  25
-- ----------------------------------------------------------------------------

letra2int :: Char -> Int
letra2int c = ord (toLower c) - 97



-- ----------------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--   int2minuscula :: Int -> Char
-- tal que '(int2minuscula n)' es la letra minúscula correspondiente al número
-- 'n', que representa una letra. Por ejemplo,
--   int2minuscula 0   ==  'a'
--   int2minuscula 3   ==  'd'
--   int2minuscula 25  ==  'z'
-- ----------------------------------------------------------------------------

int2minuscula :: Int -> Char
int2minuscula n = chr (n+97)

-- ----------------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--   int2mayuscula :: Int -> Char
-- tal que '(int2mayuscula n)' es la letra mayúscula correspondiente al número
-- 'n', que representa una letra. Por ejemplo,
--   int2mayuscula 0   ==  'A'
--   int2mayuscula 3   ==  'D'
--   int2mayuscula 25  ==  'Z'
-- ----------------------------------------------------------------------------

int2mayuscula :: Int -> Char
int2mayuscula n = chr (n+65)

-- ----------------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--   desplaza :: Int -> Char -> Char
-- tal que '(desplaza n c)' es la letra que se obtiene desplazando 'n'
-- posiciones la letra 'c'. Si 'n' es positivo el desplazamiento es hacia la
-- derecha y si 'n' es negativo el desplazamiento es hacia la izquierda. Por
-- ejemplo,
--   desplaza   3  'a'  ==  'd'
--   desplaza   3  'y'  ==  'b'
--   desplaza (-3) 'd'  ==  'a'
--   desplaza (-3) 'b'  ==  'y'
--   desplaza   3  'A'  ==  'D'
--   desplaza   3  'Y'  ==  'B'
--   desplaza (-3) 'D'  ==  'A'
--   desplaza (-3) 'B'  ==  'Y'
-- ----------------------------------------------------------------------------

desplaza :: Int -> Char -> Char
desplaza n c = if isUpper c
  then int2mayuscula (((letra2int c)+n)`mod`26)
  else int2minuscula (((letra2int c)+n)`mod`26)


-- ----------------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--   codifica :: Int -> String -> String
-- tal que '(codifica n xs)' es el resultado de codificar el texto 'xs' con un
-- desplazamiento 'n'. Por ejemplo,
--   codifica 3 "En Todo La Medida"     ==  "Hq Wrgr Od Phglgd"
--   codifica (-3) "Hq Wrgr Od Phglgd"  ==  "En Todo La Medida"
-- ----------------------------------------------------------------------------

codifica :: Int -> String -> String
codifica n xs= map (\x -> if isAlpha x then desplaza n x else x)xs

-- ============================================================================
-- Decodificación
-- ============================================================================

-- Para decodificar un mensaje codificado con el cifrado César se puede optar
-- por examinar todas las posibles transformaciones del mensaje codificado o se
-- puede usar una técnica analítica que consiste en comparar las distribuciones
-- de frecuencias de las letras del mensaje cifrado con la distribución de
-- frecuencias de las letras en el idioma usado en el mensaje. Por ejemplo, en
-- castellano, las frecuencias de las letras más frecuentes son E y A, y las
-- menos frecuentes son la K y la W, por tanto se deben corresponder con las
-- letras más frecuentes y menos frecuentes (respectivamente) en el mensaje
-- cifrado.

-- Para comparar dos distribuciones de frecuencias de una variable aleatoria
-- de n valores, la distribución observada os (la del mensaje cifrado) y la
-- distribución esperada es (la del idioma usado en el lenguaje), se puede
-- utilizar la medida Chi2 (Chi cuadrado), definida como sigue:
--
--          _n-1_
--          \       (os[i]-es[i])²
--   chi2 =  >    ------------------
--          /____        es[i]
--           i=0

-- ----------------------------------------------------------------------------
-- La constante 'tabla' es la lista de la frecuencias de las letras en
-- castellano. Por ejemplo, la frecuencia de la 'a' es del 12.53% y la de la
-- 'b' es 1.42%. La letra más frecuente es la 'e' con un 13.86% y la menos
-- frecuente es la 'k' con un 0.01%.
-- ----------------------------------------------------------------------------

tabla :: [Float]
tabla = [12.53, 1.42, 4.68, 5.86, 13.68, 0.69, 1.01, 0.70, 6.25, 0.44, 0.01,
          4.97, 3.15, 6.71, 8.68,  2.51, 0.88, 6.87, 7.98, 4.63, 3.93, 0.90,
          0.02, 0.22, 0.90, 0.52]

tablaAux :: String
tablaAux = "abcdefghijklmnopqrstuvwxyz"

-- ----------------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--   porcentaje :: Int -> Int -> Float
-- tal que '(porcentaje n m)' es el porcentaje de 'n' sobre 'm'. Por ejemplo,
--   porcentaje 2 5  ==  40.0
-- ----------------------------------------------------------------------------

porcentaje :: Int -> Int -> Float
porcentaje n m = 100*(fromIntegral n)/(fromIntegral m)

-- ----------------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--   letras :: String -> String
-- tal que '(letras xs)' es la cadena formada por las letras de la cadena
-- 'xs'. Por ejemplo,
--   letras "Esto Es Una Prueba"  ==  "EstoEsUnaPrueba"
-- ----------------------------------------------------------------------------

letras :: String -> String
letras xs = filter (isAlpha) xs

-- ----------------------------------------------------------------------------
-- Ejercicio 8. Definir la función
--   ocurrencias :: Char -> String -> Int
-- tal que '(ocurrencias x xs)' es el número de veces que aparece el carácter
-- 'x' en la cadena 'xs', independientemente de si aparece en minúsculas o en
-- mayúsculas. Por ejemplo,
--   ocurrencias 'a' "Salamanca"  ==  4
--   ocurrencias 's' "Salamanca"  ==  1
-- ----------------------------------------------------------------------------

ocurrencias :: Char -> String -> Int
ocurrencias c xs = length (filter (\x -> toLower x == toLower c) xs)

-- ----------------------------------------------------------------------------
-- Ejercicio 9. Definir la función
--   frecuencias :: String -> [Float]
-- tal que '(frecuencias xs)' es la distribución de frecuencias de las letras
-- del alfabeto en la cadena 'xs'. Por ejemplo,
--   frecuencias "En Todo La Medida"  ==
--     [14.3,0,0,21.4,14.3,0,0,0,7.1,0,0,7.1,
--      7.1,7.1,14.3,0,0,0,0,7.1,0,0,0,0,0,0]
-- ----------------------------------------------------------------------------

frecuencias :: String -> [Float]
frecuencias xs =
   map (\ c -> porcentaje (ocurrencias c xs) (length (letras xs))) ['a'..'z']

-- ----------------------------------------------------------------------------
-- Ejercicio 10. Definir la función
--   chiCuad :: [Float] -> [Float] -> Float
-- tal que '(chiCuad os es)' es la medida chi cuadrado de las distribuciones
-- 'os' y 'es'. Por ejemplo,
--   chiCuad [3,5,6] [3,5,6]  ==  0.0
--   chiCuad [3,5,6] [5,6,3]  ==  3.9666667
-- ----------------------------------------------------------------------------

chiCuad :: [Float] -> [Float] -> Float
chiCuad [] _ = 0
chiCuad _ [] = 0
chiCuad (x:xs) (y:ys) = ((x-y)^2)/y + chiCuad xs ys

-- ----------------------------------------------------------------------------
-- Ejercicio 11. Definir la función
--   rota :: Int -> [a] -> [a]
-- tal que '(rota n xs)' es la lista obtenida rotando 'n' posiciones a la
-- izquierda los elementos de la lista 'xs'. Por ejemplo,
--   rota 2 "manolo"  ==  "noloma"
-- ----------------------------------------------------------------------------

rota :: Int -> [a] -> [a]
rota n xs = [xs!!((i-1 +n)`mod`(length xs)) | i<-[1..length xs]]

-- ----------------------------------------------------------------------------
-- Ejercicio 12. Definir la función
--   posicion :: (Eq a) => a -> [a] -> Int
-- tal que '(posicion x xs)' es la posición que ocupa la primera ocurrencia del
-- elemento 'x' en la lista 'xs'. Por ejemplo,
--   posicion 'a' "Matematicas"  ==  1
--   posicion 'i' "Matematicas"  ==  
-- ----------------------------------------------------------------------------

posicion :: (Eq a) => a -> [a] -> Int
posicion n xs = fst(head(filter (\(x,y) -> y== n) (zip [0..length xs -1] xs)))

-- ----------------------------------------------------------------------------
-- Ejercicio 13. Definir la función
--   descifra :: String -> String
-- tal que '(descifra xs)' es la cadena obtenida descodificando la cadena 'xs'
-- por el desplazamiento contrario al que produce una distribución de letras
-- con la menor desviación chi cuadrado respecto de la tabla de distribución de
-- las letras en castellano. Por ejemplo,
--   codifica 5 "Todo Para Nada"  ==  "Ytit Ufwf Sfif"
--   descifra "Ytit Ufwf Sfif"    ==  "Todo Para Nada"
-- ----------------------------------------------------------------------------

descifra :: String -> String
descifra = undefined

-- ============================================================================
