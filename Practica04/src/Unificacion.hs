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
dominio [] = []
dominio (x:xs) = [fst x] ++ dominio xs


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
aplicaT s (T n []) = (V n)
aplicaT (y:ys) (T n (x:xs)) = if fst y == x then (T n (fst y:xs)) else aplicaT ys (T n (x:xs))
{- 
aplicaT s t = if esVariable(t) then aplicaVar s t else T "p" []
--aplicaT = error "D:"
-- V Nombre
-- | T Nombre [Termino]

--variables (V n) = [V n]
variables (T n []) = []
variables (T n (x:xs)) = variables x `union` variables (T n xs)
-}

{-
s1 =               [(z, f [x, y]), (x, a)]
aplicaT1 = aplicaT      s1       (g [f [x, y],      z])
--            Regresa:                   g [f [a, y], f [x, y]]
-}


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
composicion = error "D:"
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
unifica = error "D:"
-- unifica (V x) (V y) =  if x == y then [epsilon] else (V x, V y)
-- unifica (V x) t2    =  variables t2 
-- unifica (V x) (V y) = if x == y then nada else (V x) (V y), regresar epsilon
-- unifica (V x) t2 = x \not in (sacamos variables de t2), si se cumple agregamos a (x, t2)
-- unifica t1 (V y) = igual k arriba
-- unifica t1 t2 = unificar las lista xd pero solo si se cumple una condicion: que se llamen igual

-- (T f xs) (T g ys) = si f == g then unificalista


-- tenemos que recibir dos terminos, 4 casos: var, var; var, termino; termino, var; temrino, termino
-- lo primero es si recibimos las misma variables, si son las mismas no tiene caso umg
-- si no son las mismas entonces la susti que podemos dar es que cada vez que encontremos la primer var
-- ponemos la segunda la car

-- cuando tenemos que ambos son var

{-
a = T "a" []
x = V "x"
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
-}


-- Función que regresa la lista formada por el unificador más general
-- de las listas de términos.
unificaListas :: [Termino] -> [Termino] -> [Sustitucion]
unificaListas = error "D:"

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
-- Regresa: [x,y,z], 

dominio1 = dominio s1
-- Regresa: [x,z]

aplicaVar1 = aplicaVar s1 x
-- Regresa: a

aplicaVar2 = aplicaVar s1 y
-- Regresa: y

aplicaVar3 = aplicaVar s1 z
-- Regresa: f [x, y]

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

unificaListas2 = unificaListas [x, f[x]] [y, y]
-- Regresa: []
