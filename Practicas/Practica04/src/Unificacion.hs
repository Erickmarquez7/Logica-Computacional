module Unificacion where

import Data.List

-- Representa las cadenas.
type Nombre = String

-- Un término es una variable o un símbolo de función seguido de una
-- lista de términos.
data Termino = V Nombre
             | T Nombre [Termino]
             deriving Eq

-- Instancia show para terminos.
instance Show Termino where
  show (V nombre)    = nombre
  show (T nombre []) = nombre
  show (T nombre ts) = nombre ++ concat [show ts]

-- Una variable es un termino.
type Variable = Termino

-- Una sustitución es un par formado por una variable y un término.
type Sustitucion = [(Variable, Termino)]

-- Función que nos dice si un un termino es una variable.
esVariable :: Termino -> Bool
esVariable (V n) = True
esVariable (T n l) = False
--esVariable (T n (x:xs)) = False

-- Función que dado un término, regresa su lista de variables.
variables :: Termino -> [Variable]
variables (V n) = [V n]
variables (T n []) = []
variables (T n (x:xs)) = variables x `union` variables (T n xs)


-- Función que regresa la lista de variables dada una lista de términos.
variablesEnLista :: [Termino] -> [Variable]
variablesEnLista [] = []
variablesEnLista (x:xs) = variables x `union` variablesEnLista xs   


-- Función que representa a la sustitución identidad.
-- type Sustitucion = [(Variable, Termino)]
epsilon :: Sustitucion
epsilon = []


-- Función que dada una sustitución, obtenemos su dominio.
dominio :: Sustitucion -> [Variable]
dominio = map fst
--dominio [] = []
-- dominio (x:xs) = [fst x] ++ dominio xs


-- Función que dada una sustitución y una variable, regresa la
-- aplicación de la sustitución a la variable.
aplicaVar :: Sustitucion -> Variable -> Termino
aplicaVar [] x = x
aplicaVar (x:xs) v = if  fst x == v then snd x else aplicaVar xs v --T "p" []-- aplicaVar (xs t)


-- Función que dada una sustitución y un término, regresa la
-- aplicación de la sustitución al término.
aplicaT :: Sustitucion -> Termino -> Termino
aplicaT [] x = x
aplicaT s (V n) = aplicaVar s (V n)
aplicaT s (T n xs) = T n [aplicaT s x | x <- xs] --aplicamos la rec a cada uno de los elem de la lista


-- Función que regresa la sustitución obtenida, eliminando los pares
-- cuyos elementos son iguales.
reduce :: Sustitucion -> Sustitucion
reduce [] = []
reduce (x:xs) = if reduce' (fst x) (snd x) then reduce xs else x:reduce xs


-- Funcion auxiliar para ver si coinciden
reduce' :: Variable -> Termino -> Bool
reduce' (V x) (T y l) = x == y
reduce' (V x) (V y) = x == y


-- Función que dadas dos sustituciones, regresa su composición.
-- type Sustitucion = [(Variable, Termino)]
composicion :: Sustitucion -> Sustitucion -> Sustitucion
composicion xs ys = 
  (reduce [ ((fst x),(aplicaT ys (snd x))) | x <-  xs ]) ++ --tomamos los elementos de s1 y lo aplicamos
        -- a s2 quitando los repetidos
  [ a | a <- ys, (fst a) `notElem` (dominio xs) ] --tomar las sustituciones y verificar que no este en el dom


-- Función que dados dos términos, regresa la lista formada por el
-- unificador más general de ambos términos. Si no son unificables,
-- regresa la lista vacía.

unifica :: Termino -> Termino -> [Sustitucion]
unifica (V x) (V y) = if x == y then [epsilon] else [[(V x, V y)]] -- Si son iguales no hay susti
unifica (V x) t2    = if ((V x) `notElem` variables t2) then [[(V x,t2)]] else [] --si no esta en las var aplicamos la susti
unifica  t1 (V x)   = if ((V x) `notElem` variables t1) then [[(V x,t1)]] else [] -- mismo caso
unifica (T x l1) (T y l2) = [l | x == y, l <- unificaListas l1 l2] -- unificamos pero solo si tienen el mismo nombre



-- Función que regresa la lista formada por el unificador más general
-- de las listas de términos.
unificaListas :: [Termino] -> [Termino] -> [Sustitucion]
unificaListas [] [] = [epsilon] -- regresar la identidad
unificaListas l [] = [] -- no c puede, regresamos el vacio
unificaListas [] l = [] -- no c puede, regresamos el vacio
unificaListas (x:xs) (y:ys) = -- cuando son dos listas
   [composicion l2 l1 | l1 <- unifica x y, -- unificacion de las cabezas
                        l2 <- unificaListas [aplicaT l1 x | x <- xs] [aplicaT l1 y | y <- ys]]
-- la comp de dos listas  -- unificacion de las listas, dos aplicaciones de la lista anterior                


---------------------------------------------------------------------------------
--------                             EJEMPLOS                            --------
---------------------------------------------------------------------------------

-- Ejemplos de variables.

x = V "x"
y = V "y"
z = V "z"
u = V "u"
w = V "w"

-- Ejemplos de constantes.

a = T "a" []
b = T "b" []
c = T "c" []

-- Ejemplos de simbolos de función.

f = T "f"
g = T "g"
h = T "h"
p = T "p"

-- Ejemplos de sustituciones
s1 = [(x, a), (z, f [x, y])]
s2 = [(x, z), (y, u)]
s3 = [(z, x), (x, b), (u, c)]
s4 = [(u, f [x]), (y, a)]
s5 = [(x, h [z]), (y, g [b])]

-- Ejemplos de las notas

ejemplo1 = unificaListas [w1] [w2]
w1 = f [w, f [x, h [z]]]
w2 = f [g [x], f [x, y]]

ejemplo2 = unificaListas [y1] [y2]
y1 = p [x, f [y]]
y2 = p [g [y, a], f [b]]


-- ejemplos funciones

esVariable1 = esVariable x
-- Regresa: True

esVariable2 = esVariable a
-- Regresa: False

variables1 = variables (g [f [x,y], z])
-- Regresa: [x,y,z]

variablesEnLista1 = variablesEnLista [f [x,y], g [f [x, y], z]]
-- Regresa: [x,y,z]

dominio1 = dominio s1
-- Regresa: [x,z]

aplicaVar1 = aplicaVar s1 x
-- Regresa: a

aplicaVar2 = aplicaVar s1 y
-- Regresa: y

aplicaVar3 = aplicaVar s1 z
-- Regresa: f [x, y]

-- s1 = [(x, a), (z, f [x, y])]
aplicaT1 = aplicaT s1 (g [f [x, y], z])
-- Regresa: g [f [a, y], f [x, y]]

reduce1 = reduce [(x,a), (y,y), (z, f [x,y])]
-- Regresa: [(x,a), (z, f [x,y])]

composicion1 = composicion s2 s3
-- Retresa: [(y, c), (z, x), (u, c)]

composicion2 = composicion s4 s5
-- Retresa: [(u, f [h [z]]), (y, a), (x, h [z])]

unifica1 = unifica a a
-- Regresa: [[]]

unifica2 = unifica x a
-- Regresa: [[(x, a)]]

unifica3 = unifica x (f[y])
-- Regresa: [[(x, f[y])]]

unifica4 = unifica x (f[x])
-- Regresa: []

unifica5 = unifica (f[y]) x
-- Regresa: [[(x, f[y])]]

unifica6 = unifica (f[x]) x
-- Regresa: []

unificaListas1 = unificaListas [x, f[x], y] [a, y, z]
-- Regresa: [[(z, f[a]), (y, f[a]), (x, a)]]
-- La nuestra regresa: [[(x,a)],[(y,f[x])],[(y,z)],[]]

unificaListas2 = unificaListas [x, f[x]] [y, y]
-- Regresa: []
