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
-- Ter = V n | T n [Ter] -|- x, y, x ; f(x), g(y)
-- type Sustitucion = [(Variable, Termino)]
aplicaT :: Sustitucion -> Termino -> Termino
aplicaT [] x = x
aplicaT s (V n) = aplicaVar s (V n)
aplicaT s (T n xs) = T n [aplicaT s x | x <- xs]


-- Función que regresa la sustitución obtenida, eliminando los pares
-- cuyos elementos son iguales.
reduce :: Sustitucion -> Sustitucion
reduce [] = []
reduce (x:xs) = if reduce' (fst x) (snd x) then reduce xs else x:reduce xs

reduce' :: Variable -> Termino -> Bool
reduce' (V x) (T y l) = x == y
reduce' (V x) (V y) = x == y


-- Función que dadas dos sustituciones, regresa su composición.
-- type Sustitucion = [(Variable, Termino)]
composicion :: Sustitucion -> Sustitucion -> Sustitucion
composicion xs ys = 
  (reduce [ ((fst x),(aplicaT ys (snd x))) | x <-  xs ]) ++ 
  [ a | a <- ys, (fst a) `notElem` (dominio xs) ]


--  aplicaT :: Sustitucion -> Termino -> Termino
-- composicion l (y:ys) = domi (reduce (fst y, aplicaT l (snd y)))

-- La primer parte tenemos la sustitutcion s1 y s2
-- tomamos los elementos de s1 y lo aplicamos a s2, aunque este cause casos como (x,x) ya sabemos como quitarlos
-- Lo anterior lo hacemos con aplicaT
-- Luego tomar las sustituciones de s2 de la forma (x,t) (cada elemento de la lista en 
-- su patron x:xs, fst x)
-- y verificar que x no esté en el dominio de las sustituciones de s1
-- para eso tenemos la funcion de dominio
-- Que segun está en la hora 1:13 de la clase 9



-- Función que dados dos términos, regresa la lista formada por el
-- unificador más general de ambos términos. Si no son unificables,
-- regresa la lista vacía.

-- type Sustitucion = [(Variable, Termino)]
unifica :: Termino -> Termino -> [Sustitucion]
unifica (V x) (V y) = if x == y then [epsilon] else [[(V x, V y)]]
unifica (V x) t2    = if ((V x) `notElem` variables t2) then [[(V x,t2)]] else []
unifica  t1 (V x)   = if ((V x) `notElem` variables t1) then [[(V x,t1)]] else []
unifica (T x l1) (T y l2) = [l | x == y, l <- unificaListas l1 l2]

--[], [1,2]]
--unifica = error "D:"
-- unifica (V x) (V y) =  if x == y then [epsilon] :relse (V x, V y)
-- unifica (V x) t2    =  variables t2 
-- unifica (V x) t2 = x \not in (sacamos variables de t2), si se cumple agregamos a (x, t2)
-- unifica t1 (V y) = igual k arriba
-- unifica t1 t2 = unificar las lista xd pero solo si se cumple una condicion: que se llamen igual


-- Función que regresa la lista formada por el unificador más general
-- de las listas de términos.
unificaListas :: [Termino] -> [Termino] -> [Sustitucion]
unificaListas [] [] = [epsilon]
unificaListas l [] = []
unificaListas [] l = []
unificaListas (x:xs) (y:ys) = (unifica x y) ++ (unificaListas xs ys)


--unificaListas l [] = --regresa vacio, ¿cómo se representa el vacío?
--unificaListas [] l = --regresa vacio

{-  si recibimos ambas listas podemos hacer una caza de patrones de ambas listas 
si ambas son vacio entonces regresa la identidad
si al menos una es vacia entonces no se puede crear un umg y regresamos el vacio 
OJAZO: VACIO NO ES LA IDENTIDAD 

El caso interesante es cuando no son vaacias, HINTAZO xd siempre regresamos una lista de sustituciones,
las sustituciones son las listas

Entonces hay que componer dos listas, la primera es la unificacion de las cabezas de los terminos,
literalmente lo que recibimos son las listas, entonces vamos uno a uno unificando.
La segunda parte de la composicion es una unificacion de listas ¿Cuales? pues dos aplicaciones de la
lista anterior que creamos (La de la composicion) con la cabeza de cada una de las listas de las que 
nos pasan, hay que tomar todos los temrinos de la lista y a cada uno aplicarles dicha lista

-}

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
