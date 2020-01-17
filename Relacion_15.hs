-- I1M: Relación 15
-- Funciones de entrada/salida: El juego Nim
-- Departamento de Ciencias de la Computación e Inteligencia Artificial
-- Universidad de Sevilla
-- ============================================================================

-- ============================================================================
-- Librerías auxiliares
-- ============================================================================

import Data.Char
import Data.List
import Data.Bits

-- ============================================================================
-- El Nim es un juego en el que se dispone de un conjunto de montones de
-- piezas y dos jugadores retiran alternativamente por turno cualquier cantidad
-- de piezas de un único montón. El ganador es el que retira la última pieza.
--
-- En nuestra versión del juego se pide a los jugadores que indiquen la lista
-- de números de piezas de cada montón.
-- ============================================================================

-- ----------------------------------------------------------------------------
-- Representación
-- ----------------------------------------------------------------------------

-- El conjunto de montones (que llamaremos tablero de juego o simplemente
-- tablero) se representará como una lista de números indicando el número de
-- piezas de cada montón. Con esta representación, un conjunto de montones
-- podría ser [5,4,3,2,1].

type Tablero = [Int]

-- ----------------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--   finalizado :: Tablero -> Bool
-- tal que '(finalizado t)' se verifica si 't' es el tablero de un juego
-- finalizado; es decir, sin piezas. Por ejemplo,
--   finalizado [0,0,0,0,0]  ==  True
--   finalizado [1,3,0,0,1]  ==  False
-- ----------------------------------------------------------------------------

finalizado :: Tablero -> Bool
finalizado = all (==0)

-- ----------------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--   jugada :: Tablero -> Int -> Int -> Tablero
-- tal que '(jugada t m n)' es el tablero obtenido a partir del tablero 't'
-- eliminando 'n' piezas del montón 'm'. Se considera que 'm' es un montón
-- válido para el tablero 't' con al menos 'n' piezas. Por ejemplo,
--   jugada [4,3,2,1,0] 2 1  ==  [4,2,2,1,0]
-- ----------------------------------------------------------------------------

jugada :: Tablero -> Int -> Int -> Tablero
jugada t m n =fst(splitAt (m-1) t)++[t!!(m-1)-n]++snd(splitAt m t)

-- ----------------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--   piezas :: Int -> String
-- tal que '(piezas n)' es una cadena que representa un montón con 'n' piezas.
-- Por ejemplo,
--   piezas 3  ==  " * * *"
-- ----------------------------------------------------------------------------

piezas :: Int -> String
piezas n = concat(take n (repeat " *"))

-- ----------------------------------------------------------------------------
-- Ejercicio 4. Definir la acción
--   escribeMonton :: Int -> Int -> IO ()
-- tal que '(escribeMonton m n)' muestra en pantalla el contenido del montón
-- 'm' con 'n' piezas. Por ejemplo,
--   escribeMonton 2 3  =>
--     2: * * *
-- ----------------------------------------------------------------------------

escribeMonton :: Int -> Int -> IO ()
escribeMonton m n = do putStr(show m++": ")
                       putStrLn(piezas n)

-- ----------------------------------------------------------------------------
-- Ejercicio 5. Definir la acción
--   escribeTablero :: Tablero -> IO ()
-- tal que '(escribeTablero t)' muestra en pantalla el contenido del tablero
-- 't'. Por ejemplo,
--   escribeTablero [3,4,1,0,1]  =>
--     1: * * *
--     2: * * * *
--     3: *
--     4:
--     5: *
-- ----------------------------------------------------------------------------
escribeAux :: [(Int,Int)] -> IO ()
escribeAux [] =  putStr("")
escribeAux (t:ts) = do escribeMonton (fst t) (snd t)
                       escribeAux ts


escribeTablero :: Tablero -> IO ()
escribeTablero ts = escribeAux (zip [1..(length ts)] ts) 

-- ----------------------------------------------------------------------------
-- Ejercicio 6. Definir la acción
--   leeNumero :: String -> IO Int
-- tal que '(leeNumero xs)' muestra en pantalla una nueva línea con la cadena
-- 'xs', después lee una cadena y comprueba que la cadena leida es un número
-- natural. En este caso, devuelve el número natural correspondiente y en caso
-- contrario debe mostrar un mensaje informativo y volver a ejecutarse. Por
-- ejemplo,
--   leeNumero "Qué montón eliges? "     =>
--     Qué montón eliges? 4
--     4
--   leeNumero "Cuántas piezas coges? "  =>
--     Cuantas piezas coges? c
--     ERROR: Entrada incorrecta
--     Cuantas piezas coges? 3
--     3
-- ----------------------------------------------------------------------------

leeNumero :: String -> IO Int
leeNumero xs = do putStr(xs++": ")
                  ys <-getLine
                  if all (\c -> elem c "0123456789") ys
                  then do return (read ys)
                  else do putStrLn("ERROR, lo escrito no es un número.")
                          leeNumero xs    

-- ----------------------------------------------------------------------------
-- Ejercicio 7. Definir la acción
--   eligeMonton :: Tablero -> IO Int
-- tal que '(eligeMonton t)' devuelve un número entero correspondiente a un
-- montón de los del tablero 't' que tenga un número positivo (no nulo) de
-- piezas, leido con la función 'leeNumero'. Si el valor leido no se
-- corresponde con un montón del tablero 't' o se corresponde con un montón
-- que no tiene piezas, se debe mostrar un mensaje informativo y volver a
-- ejecutarse. Por ejemplo,
--   eligeMonton [1,2,0,2,1]  =>
--     Qué montón eliges? c
--     ERROR: Entrada incorrecta
--     Qué montón eliges? 6
--     ERROR: Número de montón incorrecto
--     Qué montón eliges? 3
--     ERROR: El montón está vacío
--     Qué montón eliges? 2
--     2
-- ----------------------------------------------------------------------------

eligeMonton :: Tablero -> IO Int
eligeMonton ts = do res <- leeNumero "¿Qué montón eliges?"
                    if res > length ts || ts!!(res-1) == 0
                    then do putStrLn("ERROR")
                            eligeMonton ts
                    else return (ts!!(res-1))
                  

-- ----------------------------------------------------------------------------
-- Ejercicio 8. Definir la acción
--   eligePiezas :: Tablero -> Int -> IO Int
-- tal que '(eligePiezas t m)' devuelve un número entero correspondiente a un
-- conjunto de piezas del montón 'm' en el tablero 't', leido con la función
-- 'leeNumero'. Si el valor leido es cero o excede del número de piezas del
-- montón 'm' en el tablero 't', se debe mostrar un mensaje informativo y
-- volver a ejecutarse. Por ejemplo,
--   eligePiezas [1,2,0,2,1] 2  =>
--     Cuántas piezas coges? c
--     ERROR: Entrada incorrecta
--     Cuántas piezas coges? 0
--     ERROR: Número de piezas incorrecto
--     Cuántas piezas coges? 4
--     ERROR: Número de piezas incorrecto
--     Cuántas piezas coges? 2
--     2
-- ----------------------------------------------------------------------------

eligePiezas :: Tablero -> Int -> IO Int
eligePiezas ts n = do res <- leeNumero "¿Cuántas piezas coges?"
                      if res == 0 || ts!!(n-1) < res
                        then do putStrLn("ERROR")
                                eligePiezas ts n
                        else return res

-- ----------------------------------------------------------------------------
-- Ejercicio 9. Los jugadores se representan por los números 1 y 2. Definir
-- la función
--   cambiaJugador :: Int -> Int
-- tal que '(cambiaJugador j)' es el jugador distinto a 'j'.
-- ----------------------------------------------------------------------------

cambiaJugador :: Int -> Int
cambiaJugador 1 = 2
cambiaJugador 2 = 1

-- ----------------------------------------------------------------------------
-- Ejercicio 10. Definir la acción
--   juegoNim :: Tablero -> Int -> IO ()
-- tal que '(juegoNim t j)' es el juego para dos jugadores desarrollado a
-- partir del tablero 't' y el turno del jugador 'j'. Por ejemplo,
--   juegoNim [0,1,0,1,0] 2  =>
--
--     1:
--     2: *
--     3:
--     4: *
--     5:
--
--     Turno del jugador 2
--     Qué montón eliges? 2
--     Cuántas piezas coges? 1
--
--     1:
--     2:
--     3:
--     4: *
--     5:
--
--     Turno del jugador 1
--     Qué montón eliges? 4
--     Cuántas piezas coges? 1
--
--     1:
--     2:
--     3:
--     4:
--     5:
--
--     Ha ganado el jugador 1
-- ----------------------------------------------------------------------------

juegoNim :: Tablero -> Int -> IO ()
juegoNim = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 11. Definir la función
--   listaNumeros :: String -> [Int]
-- tal que '(listaNumeros xs)' es la lista de números enteros positivos que
-- forman la cadena 'xs', separados por espacios en blanco. En caso de que la
-- cadena 'xs' contenga algo que no sea un número o espacios en blanco, debe
-- devolver la lista vacía. Por ejemplo,
--   listaNumeros "1 2 3 4 5"  ==  [1,2,3,4,5]
--   listaNumeros "1 2 a b c"  ==  []
--   listaNumeros ""           ==  []
-- ----------------------------------------------------------------------------

listaNumeros :: String -> [Int]
listaNumeros = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 12. Definir la acción
--   nim :: IO ()
-- consistente en una partida del Nim entre dos jugadores. Por ejemplo,
--   nim  =>
--     Escribe la lista de números de piezas de cada montón: a b
--     ERROR: La lista de números es incorrecta
--     Escribe la lista de números de piezas de cada montón: 2 3 4
--
--     1: * *
--     2: * * *
--     3: * * * *
--
--     Turno del jugador 1
--     Qué montón eliges? 3
--     Cuántas piezas coges? 3
--
--     1: * *
--     2: * * *
--     3: *
--
--     Turno del jugador 2
--     Qué montón eliges? 3
--     Cuántas piezas coges? 1
--
--     1: * *
--     2: * * *
--     3:
--
--     Turno del jugador 1
--     Qué montón eliges? 2
--     Cuántas piezas coges? 2
--
--     1: * *
--     2: *
--     3:
--
--     Turno del jugador 2
--     Qué montón eliges? 1
--     Cuántas piezas coges? 2
--
--     1:
--     2: *
--     3:
--
--     Turno del jugador 1
--     Qué montón eliges? 2
--     Cuántas piezas coges? 1
--
--     1:
--     2:
--     3:
--
--     Ha ganado el jugador 1
-- ----------------------------------------------------------------------------

nim :: IO ()
nim = undefined

-- ----------------------------------------------------------------------------
-- En el juego del Nim, un tablero perdedor es un tablero en el que el jugador
-- siempre pierde haga lo que haga, por ejemplo [1,1] o incluso [0], siempre
-- que el contrincante juege de la mejor manera posible. Por otro lado, un
-- tablero ganador es un tablero en el que el jugador puede realizar una
-- elección de piezas que da lugar a un tablero perdedor, por ejemplo [1,2] o
-- incluso [2].
--
-- Una estrategia ganadora consiste en identificar una propiedad que se cumpla
-- únicamente en los tableros perdedores y tal que, ante un tablero ganador,
-- permita decidir que movimiento se ha de hacer para dar lugar a un tablero
-- perdedor. De esta forma, siempre que tengamos un tablero ganador, podremos
-- hacer un movimiento que lo transforme en un tablero perdedor, dejando dicha
-- situación al contrincante.
--
-- En el juego del Nim, la propiedad que da lugar a la estrategia ganadora
-- consiste en comprobar que en las descomposiciones como suma de potencias
-- distintas de base 2 de las cantidades de piezas de cada montón, hay una
-- cantidad par de potencias del mismo exponente. Por ejemplo,
-- ·) En el tablero [1,2,3], 1 y 2 ya son potencias de 2 y 3 = 1+2. Por tanto
--    en las descomposiciones como suma de potencias distintas de base 2 de las
--    cantidades de este tablero hay dos 1 y dos 2. De aquí que el tablero sea
--    perdedor.
-- ·) En el tablero [3,5,6], 3 = 1+2, 5 = 1+4 y 6 = 2+4. Por tanto en las
--    descomposiciones como suma de potencias distintas de base 2 de las
--    cantidades de este tablero hay dos 1, dos 2 y dos 4. De aquí que el
--    tablero sea perdedor.
--
-- Para identificar la elección adecuada que da lugar a un tablero perdedor a
-- partir de un tablero ganador hay que analizar las descomposiciones como suma
-- de potencias distintas en base 2 de todas las cantidades del tablero.
--
-- Por ejemplo, dado el tablero [3,5,7], se tiene:
--   3 = 1 + 2
--   5 = 1     + 4
--   7 = 1 + 2 + 4
-- Si quitamos las parejas de potencias iguales nos queda 1 en el tercer
-- montón. La elección adecuada sería una pieza del tercer montón.
--
-- Otro ejemplo, dado el tablero [3,6,8], se tiene:
--   3 = 1 + 2
--   6 =     2 + 4
--   8 =             8
-- Si quitamos las parejas de potencias iguales nos queda 1 en el primer
-- montón, 4 en el segundo montón y 8 en el tercer montón; un total de 13. La
-- elección adecuada sería 8-(1+4) = 3 piezas del tercer montón.
--
-- Los números se pueden tratar directamente como sumas de potencias distintas
-- de base 2 con la librería Data.Bits. El emparejamiento de potencias de base
-- 2 con el mismo exponente se consigue con la operación 'xor'. Esta función
-- nos permitirá buscar el montón y la cantidad que hay que quitar para pasar
-- de un tablero ganador a un tablero perdedor.
--
-- En los siguientes ejercicios se desarrolla esta estrategia ganadora.
-- ----------------------------------------------------------------------------

-- ----------------------------------------------------------------------------
-- Ejercicio 13: Definir la función
--   parteDesparejada :: [Int] -> Int
-- tal que '(parteDesparejada t)' es el valor total de la cantidad de
-- potencias de base 2 que están desparejadas al considerar cada número del
-- tablero 't' como suma de potencias de base 2 con distinto exponente. Esto se
-- consigue fácilmente al evaluar la función 'xor' sobre todos los elementos
-- del tablero 't' de forma consecutiva. Por ejemplo:
--   parteDesparejada [1,2,3]  ==  0
--   parteDesparejada [3,5,6]  ==  0
--   parteDesparejada [3,5,7]  ==  1
--   parteDesparejada [3,6,8]  ==  13
-- ----------------------------------------------------------------------------

parteDesparejada :: Tablero -> Int
parteDesparejada = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 14: Definir la función
--   montonDescompensado :: Tablero -> Int
-- tal que '(montonDescompensado ns)' es el número de montón en el tablero 't'
-- (contando desde 1) con 'n' piezas, tal que la operación 'xor' entre 'n' y la
-- parte desparejada del tablero 't' es una cantidad estrictamente menor que
-- 'n'. Si la parte desparejada del tablero 't' es 0, es decir, 't' es un
-- tablero perdedor, entonces la función debe devolver 0. Por ejemplo:
--   montonDescompensado [1,2,3]  ==  0
--   montonDescompensado [3,5,6]  ==  0
--   montonDescompensado [3,5,7]  ==  1
--   montonDescompensado [3,6,8]  ==  3
-- ----------------------------------------------------------------------------

montonDescompensado :: Tablero -> Int
montonDescompensado = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 15: Definir la función
--   buscaEleccionGanadora :: Tablero -> (Int,Int)
-- tal que '(buscaEleccionGanadora t)' es un par '(m,n)' donde 'm' es un número
-- de montón válido en el tablero 't', 'n' es un número de piezas menor o igual
-- que el que tiene el montón 'm' en 't' y, si el tablero 't' es ganador
-- entonces tras tomar 'n' piezas del montón 'm' en el tablero 't', el tablero
-- es perdedor. En caso de que el tablero original sea perdedor entonces se
-- devuelve el par '(m,1)' donde 'm' es un montón del tablero 't' con un número
-- de piezas maximal. Por ejemplo,
--   buscaEleccionGanadora [1,2,3]  ==  (3,1)
--   buscaEleccionGanadora [3,5,6]  ==  (3,1)
--   buscaEleccionGanadora [3,5,7]  ==  (1,1)
--   buscaEleccionGanadora [3,6,8]  ==  (3,3)
-- Nota: 'm' es el montón descompensado del tablero 't' y 'n' es la diferencia
-- entre la cantidad de piezas que hay en el montón 'm' de 't' y el resultado
-- de la operación 'xor' entre dicha cantidad y la parte desparejada de 't'.
-- ----------------------------------------------------------------------------

buscaEleccionGanadora :: Tablero -> (Int,Int)
buscaEleccionGanadora = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 16: Definir la acción
--   juegoNimC :: Tablero -> Int -> IO ()
-- tal que '(juegoNimC t j)' es el juego para un jugador contra la computadora,
-- desarrollado a partir del tablero 't' y el turno del jugador 'j' (1 si es el
-- turno del jugador y 2 si es el turno de la computadora), en el que la
-- computadora utiliza la heurística ganadora. Por ejemplo,
--   juegoNimC [3,6,8] 1 =>
--
--     1: * * *
--     2: * * * * * *
--     3: * * * * * * * *
--
--     Te toca jugar
--     Qué montón eliges? 3
--     Cuántas piezas coges? 3
--
--     1: * * *
--     2: * * * * * *
--     3: * * * * *
--
--     Me toca jugar
--
--     1: * * *
--     2: * * * * *
--     3: * * * * *
--
--     Te toca jugar
--     Qué montón eliges? 2
--     Cuántas piezas coges? 3
--
--     1: * * *
--     2: * *
--     3: * * * * *
--
--     Me toca jugar
--
--     1: * * *
--     2: * *
--     3: *
--
--     Te toca jugar
--     Qué montón eliges? 1
--     Cuántas piezas coges? 2
--
--     1: *
--     2: * *
--     3: *
--
--     Me toca jugar
--
--     1: *
--     2:
--     3: *
--
--     Te toca jugar
--     Qué montón eliges? 1
--     Cuántas piezas coges? 1
--
--     1:
--     2:
--     3: *
--
--     Me toca jugar
--
--     1:
--     2:
--     3:
--
--     He ganado
-- ----------------------------------------------------------------------------

juegoNimC :: Tablero -> Int -> IO ()
juegoNimC = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 17: Definir la acción
--   nimC :: IO ()
-- consistente en una partida del Nim entre un jugador y la computadora, que
-- usa la estrategia ganadora descrita anteriormente. Por ejemplo,
--   nimC  =>
--     Escribe la lista de números de piezas de cada montón: 3 5 7
--     Quién empieza a jugar, tú (1) o yo (2)? 2
--     Suerte, te va a hacer falta ...
--
--     1: * * *
--     2: * * * * *
--     3: * * * * * * *
--
--     Me toca jugar
--
--     1: * *
--     2: * * * * *
--     3: * * * * * * *
--
--     Te toca jugar
--     Qué montón eliges? 3
--     Cuántas piezas coges? 2
--
--     1: * *
--     2: * * * * *
--     3: * * * * *
--
--     Me toca jugar
--
--     1:
--     2: * * * * *
--     3: * * * * *
--
--     Te toca jugar
--     Qué montón eliges? 2
--     Cuántas piezas coges? 2
--
--     1:
--     2: * * *
--     3: * * * * *
--
--     Me toca jugar
--
--     1:
--     2: * * *
--     3: * * *
--
--     Te toca jugar
--     Qué montón eliges? 2
--     Cuántas piezas coges? 2
--
--     1:
--     2: *
--     3: * * *
--
--     Me toca jugar
--
--     1:
--     2: *
--     3: *
--
--     Te toca jugar
--     Qué montón eliges? 2
--     Cuántas piezas coges? 1
--
--     1:
--     2:
--     3: *
--
--     Me toca jugar
--
--     1:
--     2:
--     3:
--
--     He ganado
-- ----------------------------------------------------------------------------

nimC :: IO ()
nimC = undefined

-- ============================================================================
