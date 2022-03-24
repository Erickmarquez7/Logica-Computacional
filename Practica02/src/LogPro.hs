
-- prueba xd
{-
- Lógica Computacional 2022-2
- Profesor: Francisco Hernández Quiroz
- Ayudante: José Ricardo Desales Santos
- Ayudante: Andrea Regina García Correa
- Laboratorio: Emiliano Galeana Araujo
- Laboratorio: César Eduardo Jardines Mendoza
- Practica 2: Lógica proposicional.
- Integrantes:
- Bernal Márquez Erick         317042522
- Deloya Andrade Ana Valeria   317277582
- Perez Romero Natalia Abigail 318144265
-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module LogProp where

import Data.List

data Prop =
  Var Name
  | Neg Prop
  | Conj Prop Prop
  | Disy Prop Prop
  | Impl Prop Prop
  | Syss Prop Prop deriving Show

-- El tipo type es sinonimo
-- Variables.
type Name = String


-- Sustitución.
type Sust = [(Name, Name)]


-- Estado de una variable. (p, T/F)
-- (p 
-- T/F)
type Estado = (Name, Bool) --("el awa es verde", F)


-- Solamente muchos estados, una lista de tuplas, que son los estados [(p,B),(q,B)...]
-- [( p     (q     (r  ...
--   T/F),  T/F),  T/F)]
type Estados = [Estado]


-- Un renglon es una tupla de variables, y su asignacion (True/False) y un resultado.
-- ( [(p, T/F), (q, T/F), (r, T/F) .....], T/F )
--
-- ( [( p     (q      (r,         res
--     T/F),  T/F),   T/F) ....], T/F])
type Renglon = (Estados, Bool)


-- Una tabla es solo una lista de renglones.
-- [    ([(p, F)(q, T)(q,T)...],F), 
--     ([(p, T)(q, F)(q,T)...],T), ]
type Tabla = [Renglon]


{-- Instancia para Show
instance Show Prop where
  show (Var x)    = show x
  show (Neg x)    = "¬"++ (show x)
  show (Conj x y) = "(" ++ show x ++ " Λ " ++ show y ++ ")"
  show (Disy x y) = "(" ++ show x ++ " ∨ " ++ show y ++ ")"
  show (Impl x y) = "(" ++ show x ++ " → " ++ show y ++ ")"
  show (Syss x y) = "(" ++ show x ++ " ↔ " ++ show y ++ ")"}-}


-- Instancia para Eq
instance Eq Prop where
  (Var x)    == (Var y)      = x == y
  (Neg x)    == (Neg y)      = x == y
  (Conj x y) == (Conj x' y') = x == x' && y == y'
  (Disy x y) == (Disy x' y') = x == x' && y == y'
  (Impl x y) == (Impl x' y') = x == x' && y == y'
  (Syss x y) == (Syss x' y') = x == x' && y == y'
  _ == _ = False

--------------------------------------------------------------------------------
--------                            FUNCIONES                           --------
--------------------------------------------------------------------------------

-- | Funcion que saca todos los átomos de una proposicion.
varList :: Prop -> [Name]
varList (Var p) = [p]
varList (Neg p) = varList p
varList (Conj p q) = varList p ++ varList q
varList (Disy p q) = varList p ++ varList q
varList (Impl p q) = varList p ++ varList q
varList (Syss p q) = varList p ++ varList q


-- | Funcion que elimina las implicaciones y dobles implicaciones de
-- una proposicion.
equivalencia :: Prop -> Prop
equivalencia (Var p) = Var p
equivalencia (Neg p) = Neg (equivalencia p)
equivalencia (Conj p q) = Conj (equivalencia p) (equivalencia q)
equivalencia (Disy p q) = Disy (equivalencia p) (equivalencia q)
equivalencia (Impl p q) = Disy (Neg p) (equivalencia q)
equivalencia (Syss p q) = Conj (Disy (equivalencia p) (equivalencia q)) (Disy (Neg q) (Neg p))

-- | Funcion que niega una proposicion
negacion :: Prop -> Prop
negacion (Var p) = Neg (Var p)
negacion (Neg p) =  Neg (negacion p)
negacion (Conj p q) = Disy (negacion p) (negacion q)
negacion (Disy p q) = Conj (negacion p) (negacion q)
negacion (Impl p q) = Conj p (Neg q)
negacion (Syss p q) = Neg (Syss p q)

-- | Funcion que dada una proposicion y una sustitucion, sustituye las
-- variables que correspondan. Sust = [(Name, Name)]
sustituye :: Prop -> Sust -> Prop
sustituye (Var p) (z:zs) = if fst z == p then Var (snd z) else sustituye (Var p) zs
sustituye (Neg p) (z:zs) =  Neg (sustituye p (z:zs))
sustituye (Conj p q) (z:zs) = Conj (sustituye p (z:zs)) (sustituye q (z:zs))
sustituye (Disy p q) (z:zs) = Disy (sustituye p (z:zs)) (sustituye q (z:zs))
sustituye (Impl p q) (z:zs) = Impl (sustituye p (z:zs)) (sustituye q (z:zs))
sustituye (Syss p q) (z:zs) = Syss (sustituye p (z:zs)) (sustituye q (z:zs))



-- | Funcion que dada una proposición y estados, evalua la proposicion
-- asignando el estado que corresponda. Si no existe una variable,
-- maneja el error.
--            p || q || r  -> [(p,T), (q,F), (r,T)] ->V
interp :: Prop -> Estados -> Bool
interp (Var p) (z:zs) = if fst z == p then snd z else interp (Var p) zs
interp (Neg p) z = not (interp p z)
interp (Conj p q) z = interp p z && interp q z
interp (Disy p q) z = interp p z || interp q z
interp (Impl p q) z = not (interp p z) || interp q z
interp (Syss p q) z = interp p z == interp q z
-- es lo mismo solo que el editor me dijo que se ve mejor así xd. 
--No hagas cosas solo porque el editor lo dice, porque el editor puede importar librerias que no se supone que usemos
-- Y a veces ya no queda claro que hace.



-- | Funcion que dada una proposicion, dice True si es tautologia,
-- False en otro caso.
-- interpreta :: prop -> Estados -> bool
-- compruebaV :: Estados -> bool

-- generaEst :: [Name] -> Estados
-- varList :: Prop -> [Name] 

-- EsT Var p =  inter varP (generaEstados(varList p)) -> bool
-- Comprueba [t,t,t,t]
-- CompruebaV [inter varP (generaEstados(varList p)), ]

esTautologia :: Prop -> Bool
--   interpretamos a p <-- (Generamos los estados de la variable   <--Sacamos las variables)
esTautologia p = compruebaV [interp p (generaEstados (varList p))]

-- esTautologia (Var p) = interp (Var p) (generaEstados (varList (Var p)))
-- esTautologia (Neg p) = interp p (generaEstados (varList p))
-- esTautologia (Conj p q) = interp (Conj p q) (generaEstados (varList p)++generaEstados(varList q))
-- esTautologia (Disy p q) = interp (Disy p q) (generaEstados (varList p)++generaEstados(varList q))
-- esTautologia (Impl p q) = interp (Impl p q) (generaEstados (varList p)++generaEstados(varList q))
-- esTautologia (Syss p q) = interp (Syss p q) (generaEstados (varList p)++generaEstados(varList q))
-- idea
-- interpretar -> [t,f,t,f...] = lis
-- if (lis =t) return true
  -- else false

-- | Funcion que dada una proposicion, dice True si es una
-- contradiccion, False en otro caso.
esContradiccion :: Prop -> Bool
esContradiccion p = compruebaF [interp p (generaEstados (varList p))]
-- esContradiccion (Var p)    = not (interp (Var p) (generaEstados (varList (Var p))))
-- esContradiccion (Neg p)    = 
-- esContradiccion (Conj p q) = 
-- esContradiccion (Disy p q) = 
-- esContradiccion (Impl p q) = 
-- esContradiccion (Syss p q) = 



-- | Funcion que dada una proposicion, dice True si es satisfacible,
-- False en otro caso.
esSatisfacible :: Prop -> Bool
esSatisfacible p = not (esContradiccion p)

-- | Funcion que dada una proposicion, devuelve su tabla de verdad.

tablaDeVerdad :: Prop -> Tabla
tablaDeVerdad = error "D:"


-- | Funcion que devuelve la lista de todos los modelos posibles para
-- una proposición.
modelos :: Prop -> [Estados]
modelos = error "D:"
--------------------------------------------------------------------------------
--------                           AUXILIARES                           --------
--------------------------------------------------------------------------------
--función auxiliar para la potencia
conjPoten :: Eq a => [a] -> [[a]]
conjPoten []     = [[]]
conjPoten (x:xs) = map (x: ) pt `union` pt
  where
    pt = conjPoten xs

--función auxiliar para dar una lista de los posibles Estados de una variable, V y F para cada Proposición
--type Estado = (Name, Bool) 
--type Estado = (Name, Bool) --("el awa es verde", F) 
--type Estados = [Estado]
generaEstados:: [Name] -> Estados
generaEstados [] = []
generaEstados (x:xs) = [(x,False)]++[(x,True)]++generaEstados xs
-- [(x,V),(x,F), (y,V), (y,F) ...   ]

-- [ (), ()...]
-- Nos dice si todos los elementos de los estados son verdaderos
compruebaV:: [Bool] -> Bool
compruebaV [] = True
compruebaV (x:xs) =  x && compruebaV xs

-- Nos dice si todos los elementos de los estados son Falsos
compruebaF:: [Bool] -> Bool
compruebaF [] = True
compruebaF (x:xs) = not x && compruebaF xs


esta :: Eq a => a -> [a] -> Bool
esta _ [] = False
esta y (x:xs) = y==x || esta y xs
--------------------------------------------------------------------------------
--------                             EJEMPLOS                           --------
---------------------------------------s-----------------------------------------

imp :: Prop
imp = Impl (Var "P") (Conj (Var "Q") (Neg (Var "R")))

varList1 = varList imp
-- Regresa: ["P", "Q", "R"]

equivalencia1 = equivalencia imp
-- Regresa: Disy (Neg (Var "P")) (Conj (Var "Q") (Neg (Var "R")))

equivalencia2 = equivalencia (Syss imp imp)
-- Regresa: Conj (Disy (Neg (Disy (Neg (Var "P")) (Conj (Var "Q") (Neg
-- (Var "R"))))) (Disy (Neg (Var "P")) (Conj (Var "Q") (Neg (Var
-- "R"))))) (Disy (Neg (Disy (Neg (Var "P")) (Conj (Var "Q") (Neg (Var
-- "R"))))) (Disy (Neg (Var "P")) (Conj (Var "Q") (Neg (Var "R")))))

negacion1 = negacion imp
-- Regresa: Conj (Var "P") (Disy (Neg (Var "Q")) (Var "R"))

negacion2 = negacion (Neg imp)
-- Regresa: Impl (Var "P") (Conj (Var "Q") (Neg (Var "R")))

sustituye1 = sustituye imp [("P", "A"), ("Q", "B"), ("R", "C")]
-- Regresa: Impl (Var "A") (Conj (Var "B") (Neg (Var "C")))

interp1 = interp imp [("Q",True), ("P",False)]
-- Regresa: True
interp2 = interp imp [("Q",True), ("P",True)]
-- Regresa: error
interp3 = interp imp [("Q",True), ("P",True), ("R",True)]
-- Regresa: False

tauto :: Prop
tauto = Conj (Var "P") (Neg (Var "P"))
-- se supone es tauto

contra :: Prop
contra = Disy (Var "P") (Neg (Var "P"))
