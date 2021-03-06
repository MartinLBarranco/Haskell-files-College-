-- I1M: Relaci�n 19
-- Relaciones binarias homog�neas con la librer�a Data.Set
-- Departamento de Ciencias de la Computaci�n e Inteligencia Artificial
-- Universidad de Sevilla
-- ============================================================================

-- ============================================================================
-- Librer�as auxiliares
-- ============================================================================

import Test.QuickCheck
import Data.Set as S

-- ============================================================================
-- Representaci�n de las relaciones binarias homog�neas
-- ============================================================================

-- Una relaci�n binaria R sobre un conjunto A se puede representar mediante un
-- par (xs,ps) donde xs es el conjunto de los elementos de A (el universo de R)
-- y ps es el conjunto de pares de R (el grafo de R).

data Rel a = R (Set a) (Set (a,a))
  deriving (Eq, Show)

-- ----------------------------------------------------------------------------
-- Ejercicio 1. Definir la funci�n
--   relacion :: Ord a => [a] -> [(a,a)] -> Rel a
-- tal que '(relacion xs ps)' es la relaci�n de universo 'xs' y cuyo grafo es
-- el conjunto de pares de 'ps' cuyos elementos est�n en 'xs'. Por ejemplo,
--   relacion [1,3] [(1,1),(1,3),(3,3)]  ==
--     R (fromList [1,3]) (fromList [(1,1),(1,3),(3,3)])
--   relacion [1,3] [(1,1),(1,4),(3,3)]  ==
--     R (fromList [1,3]) (fromList [(1,1),(3,3)])
-- ----------------------------------------------------------------------------

relacion :: Ord a => [a] -> [(a,a)] -> Rel a
relacion xs ys = R (fromList xs) (fromList ys)

-- ----------------------------------------------------------------------------
-- Ejercicio 2. Definir la funci�n
--   universo :: Ord a => Rel a -> Set a
-- tal que '(universo r)' es el universo de la relaci�n 'r'. Por ejemplo,
--   universo (relacion [1..9] [(1,3),(2,6),(8,9),(2,7)])  ==
--     fromList [1,2,3,4,5,6,7,8,9]
-- ----------------------------------------------------------------------------

universo :: Ord a => Rel a -> Set a
universo (R xs ys) = fromList(elems xs)

-- ----------------------------------------------------------------------------
-- Ejercicio 3. Definir la funci�n
--   grafo :: Ord a => Rel a -> Set (a,a)
-- tal que '(grafo r)' es el grafo de la relaci�n 'r'. Por ejemplo,
--   grafo (relacion [1..9] [(1,3),(2,6),(8,9),(2,7)])  ==
--     fromList [(1,3),(2,6),(2,7),(8,9)]
-- ----------------------------------------------------------------------------

grafo :: Ord a => Rel a -> Set (a,a)
grafo (R xs ys) = fromList(elems ys)

-- ----------------------------------------------------------------------------
-- Ejercicio 4. Definir la funci�n
--   reflexiva :: Ord a => Rel a -> Bool
-- tal que '(reflexiva r)' se verifica si la relaci�n 'r' es reflexiva. Por
-- ejemplo,
--   reflexiva (relacion [1,3] [(1,1),(1,3),(3,3)])    ==  True
--   reflexiva (relacion [1,2,3] [(1,1),(1,3),(3,3)])  ==  False
-- ----------------------------------------------------------------------------

reflexiva :: Ord a => Rel a -> Bool
reflexiva (R xs ys)=S.foldr (\(x,y) -> ( && member (y,x) ys) ) True ys

-- ----------------------------------------------------------------------------
-- Ejercicio 5. Definir la funci�n
--   simetrica :: Ord a => Rel a -> Bool
-- tal que '(simetrica r)' se verifica si la relaci�n 'r' es sim�trica. Por
-- ejemplo,
--   simetrica (relacion [1,3] [(1,1),(1,3),(3,1)])  ==  True
--   simetrica (relacion [1,3] [(1,1),(1,3),(3,2)])  ==  False
--   simetrica (relacion [1,3] [])                   ==  True
-- ----------------------------------------------------------------------------

simetrica :: Ord a => Rel a -> Bool
simetrica (R u g)= all (\(x,y) -> S.member (y,x) g) g

-- ----------------------------------------------------------------------------
-- Ejercicio 6. Definir la funci�n
--   composicion :: Ord a => Rel a -> Rel a -> Rel a
-- tal que '(composicion r s)' es la composici�n de las relaciones 'r' y 's'.
-- Por ejemplo,
--   composicion (relacion [1,2] [(1,2),(2,2)])
--               (relacion [1,2,3] [(1,2),(2,3)])  ==
--     R (fromList [1,2,3]) (fromList [(1,3),(2,3)])
-- ----------------------------------------------------------------------------

composicion :: Ord a => Rel a -> Rel a -> Rel a
composicion (R u1 g1) (R u2 g2)= R (union u1 u2) (fromList [(a,d) | (a,b)<-(elems g1), (c,d)<-(elems g2), b==c])

-- ----------------------------------------------------------------------------
-- Ejercicio 7. Definir la funci�n
--   transitiva :: Ord a => Rel a -> Bool
-- tal que '(transitiva r)' se verifica si la relaci�n 'r' es transitiva.
-- Por ejemplo,
--   transitiva (relacion [1,3,5] [(1,1),(1,3),(3,1),(3,3),(5,5)])  ==  True
--   transitiva (relacion [1,3,5] [(1,1),(1,3),(3,1),(5,5)])        ==  False
-- ----------------------------------------------------------------------------

transitiva :: Ord a => Rel a -> Bool
transitiva r = composicion r r == r

-- ----------------------------------------------------------------------------
-- Ejercicio 8. Definir la funci�n
--   esEquivalencia :: Ord a => Rel a -> Bool
-- tal que '(esEquivalencia r)' se verifica si la relaci�n 'r' es de
-- equivalencia. Por ejemplo,
--   esEquivalencia (relacion [1..3] [(1,1),(1,2),(2,1),(2,2),(3,3)])  ==
--     True
--   esEquivalencia (relacion [1..5] [(1,1),(1,3),(3,1),(3,3),(5,5)])  ==
--     False
--   esEquivalencia (relacion [1,3,5] [(1,1),(1,3),(3,3),(5,5)])       ==
--     False
-- ----------------------------------------------------------------------------

esEquivalencia :: Ord a => Rel a -> Bool
esEquivalencia r = transitiva r && simetrica r && reflexiva r

-- ----------------------------------------------------------------------------
-- Ejercicio 9. Definir la funci�n
--   irreflexiva :: Ord a => Rel a -> Bool
-- tal que '(irreflexiva r)' se verifica si la relaci�n 'r' es irreflexiva;
-- es decir, si ning�n elemento de su universo est� relacionado con �l mismo.
-- Por ejemplo,
--   irreflexiva (relacion [1..3] [(1,2),(2,1),(2,3)])  ==  True
--   irreflexiva (relacion [1..3] [(1,2),(2,1),(3,3)])  ==  False
-- ----------------------------------------------------------------------------

irreflexiva :: Ord a => Rel a -> Bool
irreflexiva r = not(reflexiva r)

-- ----------------------------------------------------------------------------
-- Ejercicio 10. Definir la funci�n
--   antisimetrica :: Ord a => Rel a -> Bool
-- tal que '(antisimetrica r)' se verifica si la relaci�n 'r' es antisim�trica;
-- es decir, si '(x,y)' e '(y,x)' est�n relacionados, entonces 'x == y'. Por
-- ejemplo,
--   antisimetrica (relacion [1,2] [(1,2)])        ==  True
--   antisimetrica (relacion [1,2] [(1,2),(2,1)])  ==  False
--   antisimetrica (relacion [1,2] [(1,1),(2,1)])  ==  True
-- ----------------------------------------------------------------------------

antisimetrica :: Ord a => Rel a -> Bool
antisimetrica r = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 11. Definir la funci�n
--   total :: Ord a => Rel a -> Bool
-- tal que '(total r)' se verifica si la relaci�n 'r' es total; es decir, si
-- para cualquier par de elementos 'x' e 'y' del universo de 'r', se tiene que
-- 'x' est� relacionado con 'y' � 'y' est� relacionado con 'x'. Por ejemplo,
--   total (relacion [1,3] [(1,1),(3,1),(3,3)])  ==  True
--   total (relacion [1,3] [(1,1),(3,1)])        ==  False
--   total (relacion [1,3] [(1,1),(3,3)])        ==  False
-- ----------------------------------------------------------------------------

total :: Ord a => Rel a -> Bool
total r = all (\(x,y)->S.member (x,y) (grafo r) ||S.member (y,x) (grafo r)) [(x,y)|x<-elems(universo r), y<-elems (universo r)]

-- ----------------------------------------------------------------------------
-- Ejercicio 12. Comprobar con QuickCheck que las relaciones totales son
-- reflexivas.
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_total_reflexiva :: Rel Int -> Bool
prop_total_reflexiva = undefined

-- La comprobaci�n es
--   > quickCheck prop_total_reflexiva

-- ----------------------------------------------------------------------------
-- Ejercicio 13. Definir la funci�n
--   clausuraReflexiva :: Ord a => Rel a -> Rel a
-- tal que '(clausuraReflexiva r)' es la clausura reflexiva de 'r'; es decir,
-- la menor relaci�n reflexiva que contiene a 'r'. Por ejemplo,
--   clausuraReflexiva (relacion [1,3] [(1,1),(3,1)])  ==
--     R (fromList [1,3]) (fromList [(1,1),(3,1),(3,3)])
-- ----------------------------------------------------------------------------

clausuraReflexiva :: Ord a => Rel a -> Rel a
clausuraReflexiva = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 14. Comprobar con QuickCheck que la clausura reflexiva de una
-- relaci�n calculada con la funci�n 'clausuraReflexiva' es una relaci�n
-- reflexiva tal y como caracteriza la funci�n 'reflexiva'.
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_ClausuraReflexiva :: Rel Int -> Bool
prop_ClausuraReflexiva = undefined

-- La comprobaci�n es
--   > quickCheck prop_ClausuraReflexiva

-- ----------------------------------------------------------------------------
-- Ejercicio 15. Definir la funci�n
--   clausuraSimetrica :: Ord a => Rel a -> Rel a
-- tal que '(clausuraSimetrica r)' es la clausura sim�trica de 'r'; es decir,
-- la menor relaci�n sim�trica que contiene a 'r'. Por ejemplo,
--   clausuraSimetrica (relacion [1,3,5] [(1,1),(3,1),(1,5)])  ==
--     R (fromList [1,3,5]) (fromList [(1,1),(1,3),(1,5),(3,1),(5,1)])
-- ----------------------------------------------------------------------------

clausuraSimetrica :: Ord a => Rel a -> Rel a
clausuraSimetrica = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 16. Comprobar con QuickCheck que la clausura sim�trica de una
-- relaci�n calculada con la funci�n 'clausuraSimetrica' es una relaci�n
-- sim�trica tal y como caracteriza la funci�n 'simetrica'.
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_ClausuraSimetrica :: Rel Int -> Bool
prop_ClausuraSimetrica = undefined

-- La comprobaci�n es
--   > quickCheck prop_ClausuraSimetrica

-- ----------------------------------------------------------------------------
-- Ejercicio 17. Definir la funci�n
--   clausuraTransitiva :: Ord a => Rel a -> Rel a
-- tal que '(clausuraTransitiva r)' es la clausura transitiva de 'r'; es decir,
-- la menor relaci�n transitiva que contiene a 'r'. Por ejemplo,
--   clausuraTransitiva (relacion [1..6] [(1,2),(2,5),(5,6)])  ==
--     R (fromList [1,2,3,4,5,6])
--       (fromList [(1,2),(1,5),(1,6),(2,5),(2,6),(5,6)])
-- ----------------------------------------------------------------------------

clausuraTransitiva :: Ord a => Rel a -> Rel a
clausuraTransitiva = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 18. Comprobar con QuickCheck que la clausura transitiva de una
-- relaci�n calculada con la funci�n 'clausuraTransitiva' es una relaci�n
-- transitiva tal y como caracteriza la funci�n 'transitiva'.
-- ----------------------------------------------------------------------------

-- La propiedad es
prop_ClausuraTransitiva :: Rel Int -> Bool
prop_ClausuraTransitiva = undefined

-- La comprobaci�n es
--   > quickCheck prop_ClausuraTransitiva

-- ----------------------------------------------------------------------------
-- Ejercicio 19. Definir la funci�n
--   inyectiva :: Ord a => Rel a -> Bool
-- tal que '(inyectiva r)' se verifica si la relaci�n 'r' es inyectiva; es
-- decir, si para todo elemento 'b' del universo de la relaci�n, no existen dos
-- elementos distintos 'a1' y 'a2' tales que los pares '(a1,b)' y '(a2,b)'
-- est�n en el grafo de la relaci�n. Por ejemplo,
--   inyectiva (relacion [1..3] [(1,1),(2,2)])              ==  True
--   inyectiva (relacion [1..3] [(1,2),(2,3),(3,1)])        ==  True
--   inyectiva (relacion [1..3] [(1,1),(2,1),(2,2),(3,3)])  ==  False
--   inyectiva (relacion [1..3] [(1,1),(2,1),(3,1)])        ==  False
-- ----------------------------------------------------------------------------

inyectiva :: Ord a => Rel a -> Bool
inyectiva (R u g) = length [member (i,j) g| j<-elems u]==1

-- ----------------------------------------------------------------------------
-- Ejercicio 20. Definir la funci�n
--   sobreyectiva :: Ord a => Rel a -> Bool
-- tal que '(sobreyectiva r)' se verifica si la relaci�n 'r' es sobreyectiva;
-- es decir, si para todo elemento 'b' del universo de la relaci�n, existe un
-- elemento 'a' tal que el par '(a,b)' est� en el grafo de la relaci�n. Por
-- ejemplo,
--   sobreyectiva (relacion [1..3] [(1,1),(2,2)])              ==  False
--   sobreyectiva (relacion [1..3] [(1,2),(2,3),(3,1)])        ==  True
--   sobreyectiva (relacion [1..3] [(1,1),(2,1),(2,2),(3,3)])  ==  True
--   sobreyectiva (relacion [1..3] [(1,1),(2,1),(3,1)])        ==  False
-- ----------------------------------------------------------------------------

sobreyectiva :: Ord a => Rel a -> Bool
sobreyectiva = undefined

-- ============================================================================
-- Generador de conjuntos
-- ============================================================================

-- genRel es un generador de relaciones. Por ejemplo,
--   > sample genRel
--   R (fromList [3]) (fromList [])
--   R (fromList [1,2]) (fromList [(1,1)])
--   R (fromList [1,2,3]) (fromList [])
--   R (fromList [3,5,6]) (fromList [(6,3)])
--   R (fromList [4,5,7,8,10]) (fromList [(4,8),(5,10),(8,4),(8,8)])
--   R (fromList [4,7,8,9]) (fromList [(4,4),(8,7),(8,8),(8,9),(9,8)])
--   R (fromList [4,7,9,10,11,14])
--     (fromList [(4,9),(9,14),(10,10),(11,14),(14,14)])
--   R (fromList [3,6,10,14])
--     (fromList [(3,10),(6,3),(6,6),(6,10),(10,10),(10,14),(14,6)])
--   R (fromList [2,5,8,9,12,14,17])
--     (fromList [(2,2),(2,5),(2,8),(2,14),(5,5),(8,2),(9,8),(12,2),
--                (12,9),(14,2),(14,14)])
--   R (fromList [1,2,4,6,7,8,9,12,14,15,16,17,19])
--     (fromList [(12,6),(12,15),(14,17),(15,1),(16,17),(19,14)])
--   R (fromList [1,4,8,9,10,11,14,15,18,22])
--     (fromList [(1,9),(1,22),(8,8),(8,9),(9,1),(9,4),(10,1),(10,4),
--                (18,14),(18,22),(22,14)])

genRel :: (Arbitrary a, Integral a) => Gen (Rel a)
genRel =
  do xs <- listOf1 (suchThat arbitrary (> 0))
     ys <- listOf (elements [(x,y) | x <- xs, y <- xs])
     return (R (fromList xs) (fromList ys))

instance (Arbitrary a, Integral a) => Arbitrary (Rel a) where
  arbitrary = genRel

-- ============================================================================
