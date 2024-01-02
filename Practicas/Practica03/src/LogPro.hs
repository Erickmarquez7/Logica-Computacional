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
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}


module LogPro where

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

-- Descomentar esta clase para que sea vea mas bonito y comentar deriving Show de data Prop
-- Instancia para Show
{-instance Show Prop where
  show (Var x)    = show x
  show (Neg x)    = "¬"++ (show x)
  show (Conj x y) = "(" ++ show x ++ " Λ " ++ show y ++ ")"
  show (Disy x y) = "(" ++ show x ++ " ∨ " ++ show y ++ ")"
  show (Impl x y) = "(" ++ show x ++ " → " ++ show y ++ ")"
  show (Syss x y) = "(" ++ show x ++ " ↔ " ++ show y ++ ")"-}


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
equivalencia (Impl p q) = Disy (Neg (equivalencia p)) (equivalencia q) -- ¬P v Q
equivalencia (Syss p q) = Conj (Disy (equivalencia p) (equivalencia q)) (Disy (Neg ( equivalencia q)) (Neg (equivalencia p)))


eliminaNeg :: Prop -> Prop
--eliminaNeg (PNeg (PTrue)) = PFalse
--eliminaNeg (PNeg (PFalse)) = PTrue
eliminaNeg (Neg (Var p)) = (Neg (Var p))
eliminaNeg (Neg (Neg p)) = eliminaNeg p
eliminaNeg (Neg (Disy p q)) = eliminaNeg (deMorgan (Neg (Disy p q)))
eliminaNeg (Neg (Conj p q)) = eliminaNeg (deMorgan (Neg (Conj p q)))
eliminaNeg (Neg (Impl p q)) = eliminaNeg (Conj p (Neg q))
eliminaNeg (Neg (Syss p q)) = eliminaNeg (Syss (Neg p)(q))
--eliminaNeg (PTrue) = PTrue
--eliminaNeg (PFalse) = PFalse
eliminaNeg (Var p) = Var p
--eliminaNeg (Neg p) = eliminaNeg (Neg (eliminaNeg p))
eliminaNeg (Disy p q) = Disy (eliminaNeg p) (eliminaNeg q)
eliminaNeg (Conj p q) = Conj (eliminaNeg p) (eliminaNeg q)
eliminaNeg (Impl p q) = Impl (eliminaNeg p) (eliminaNeg q)
eliminaNeg (Syss p q) = Syss (eliminaNeg p) (eliminaNeg q)

deMorgan :: Prop -> Prop
deMorgan (Neg (Disy a b)) = (Conj (deMorgan(Neg a)) (deMorgan(Neg b)))
deMorgan (Neg (Conj a b)) = (Disy (deMorgan(Neg a)) (deMorgan(Neg b)))
deMorgan p = p


-- | Funcion que niega una proposicion
negacion :: Prop -> Prop
negacion (Var p)    = Neg (Var p)
negacion (Neg p)    = p
negacion (Conj p q) = Disy (negacion p) (negacion q) -- Por Morgan
negacion (Disy p q) = Conj (negacion p) (negacion q)
negacion (Impl p q) = Conj p (negacion q) -- pasamos a su eq y la negamos
negacion (Syss p q) = Disy (negacion (Impl p q)) (negacion (Impl q p)) --de la misma manera sacamos eq y la negamos
-- en principio es ¬syss p q -> ¬{p->q y q->p} -> {¬(p->q) o ¬(q->p)}  por eso nos quedamos con la disy


-- | Funcion que dada una proposicion y una sustitucion, sustituye las
-- variables que correspondan. Sust = [(Name, Name)]
sustituye :: Prop -> Sust -> Prop
sustituye (Var _) []    = error "Alguna variable no coincide con la variable a sustituir"
sustituye (Neg _) []    = error "Alguna variable no coincide con la variable a sustituir"
sustituye (Conj _ _) [] = error "Alguna variable no coincide con la variable a sustituir"
sustituye (Disy _ _) [] = error "Alguna variable no coincide con la variable a sustituir"
sustituye (Impl _ _) [] = error "Alguna variable no coincide con la variable a sustituir"
sustituye (Syss _ _) [] = error "Alguna variable no coincide con la variable a sustituir"
sustituye (Var p) (z:zs) = if fst z == p then Var (snd z) else sustituye (Var p) zs
sustituye (Neg p) (z:zs) =  Neg (sustituye p (z:zs))
sustituye (Conj p q) (z:zs) = Conj (sustituye p (z:zs)) (sustituye q (z:zs))
sustituye (Disy p q) (z:zs) = Disy (sustituye p (z:zs)) (sustituye q (z:zs))
sustituye (Impl p q) (z:zs) = Impl (sustituye p (z:zs)) (sustituye q (z:zs))
sustituye (Syss p q) (z:zs) = Syss (sustituye p (z:zs)) (sustituye q (z:zs))


-- | Funcion que dada una proposición y estados, evalua la proposicion
-- asignando el estado que corresponda. Si no existe una variable,
-- maneja el error.
interp :: Prop -> Estados -> Bool
interp (Var _) [] = error "Insuficientes Valores"
interp (Var p) (z:zs) = if fst z == p then snd z else interp (Var p) zs
interp (Neg p) z = not (interp p z)
interp (Conj p q) z = interp p z && interp q z
interp (Disy p q) z = interp p z || interp q z
interp (Impl p q) z = not (interp p z) || interp q z
interp (Syss p q) z = interp p z == interp q z


-- | Funcion que dada una proposicion, dice True si es tautologia,
-- False en otro caso.
-- Tabla -> [Renglon]
-- renglon -> (Estados, bool)
esTautologia :: Prop -> Bool
esTautologia p = compruebaV valores
  where
    valores = [snd x | x <- renglones]
    renglones = tablaDeVerdad p

-- | Funcion que dada una proposicion, dice True si es una
-- contradiccion, False en otro caso.
esContradiccion :: Prop -> Bool
esContradiccion p = compruebaF valores
  where
    valores = [snd x | x<- renglones]
    renglones = tablaDeVerdad p



-- | Funcion que dada una proposicion, dice True si es satisfacible,
-- False en otro caso. Por definción satisfacible es solo que no sea contradicción
esSatisfacible :: Prop -> Bool
esSatisfacible p = not (esContradiccion p)


-- | Funcion que dada una proposicion, devuelve su tabla de verdad.
tablaDeVerdad :: Prop -> Tabla
tablaDeVerdad p = [(edos, interp p edos) | edos <- mods]
  where
    mods = estados p


-- | Funcion que devuelve la lista de todos los modelos posibles para
-- una proposición.
modelos :: Prop -> [Estados]
modelos p = filter (interp p) [fst x | x <-  tablaDeVerdad p]
-- tabla = [renglones]
-- renglon = (Estados, bool)
-- Estados = [Estado]
-- Estado = (Name, Bool)

--------------------------------------------------------------------------------
--------                           AUXILIARES                           --------
--------------------------------------------------------------------------------

--función auxiliar para la potencia
conjPoten :: Eq a => [a] -> [[a]]
conjPoten []     = [[]]
conjPoten (x:xs) = map (x: ) pt `union` pt
  where
    pt = conjPoten xs

-- Nos da todos los estados de una Proposición
estados :: Prop -> [Estados]
estados p = map sort $ zipWith (++) true' false'
  where
    true = conjPoten $ varList p
    true' = map (\x -> (map (\y -> (y, True)) x)) true
    false = reverse $ conjPoten $ varList p
    false' = map (\x -> (map (\y -> (y, False)) x)) false


-- Nos dice si todos los elementos de los estados son verdaderos
compruebaV:: [Bool] -> Bool
compruebaV [] = True
compruebaV (x:xs) =  x && compruebaV xs

-- Nos dice si todos los elementos de los estados son Falsos
compruebaF:: [Bool] -> Bool
compruebaF [] = True
compruebaF (x:xs) = not x && compruebaF xs


--------------------------------------------------------------------------------
--------                             EJEMPLOS                           --------
--------------------------------------------------------------------------------

imp :: Prop
imp = Impl (Var "P") (Conj (Var "Q") (Neg (Var "R")))

-- P -> (Q v ¬R)

-- Ejemplo de 
varList1 = varList imp
-- Regresa: ["P", "Q", "R"]

equivalencia1 = equivalencia imp
-- Regresa: Disy (Neg (Var "P")) (Conj (Var "Q") (Neg (Var "R")))

equivalencia2 = equivalencia (Syss imp imp)
-- Regresa: Conj (Disy (Neg (Disy (Neg (Var "P")) (Conj (Var "Q") (Neg
-- (Var "R"))))) (Disy (Neg (Var "P")) (Conj (Var "Q") (Neg (Var
-- "R"))))) (Disy (Neg (Disy (Neg (Var "P")) (Conj (Var "Q") (Neg (Var
-- "R"))))) (Disy (Neg (Var "P")) (Conj (Var "Q") (Neg (Var "R")))))

prueba = Neg (Conj (Neg (Var "P")) (Neg (Var "Q")) )
pruebaEquiv = eliminaNeg prueba

negacion1 = negacion imp
-- Regresa: Conj (Var "P")  (Disy (Neg (Var "Q")) (Var "R"))


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

tauto1 = esTautologia imp
-- Regresa: False

contra1 = esContradiccion imp

-- Ejemplos de tautologia y contradicción
t :: Prop
t = Disy (Var "P") (Neg (Var "P")) -- P v ¬P

c :: Prop
c = Neg (Disy (Var "P") (Neg (Var "P")))

t1 = esTautologia t
-- Regresa: True

c1 = esContradiccion c
-- Regresa: True

satis = esSatisfacible c
-- Regresa: False 

tabla = tablaDeVerdad imp
{- regresa: [([("P",True),("Q",True),("R",True)],False),([("P",True),("Q",True),("R",False)],True),
          ([("P",True),("Q",False),("R",True)],False),([("P",True),("Q",False),("R",False)],False),
          ([("P",False),("Q",True),("R",True)],True),([("P",False),("Q",True),("R",False)],True),
          ([("P",False),("Q",False),("R",True)],True),([("P",False),("Q",False),("R",False)],True)] -}

modelo = modelos imp
{- Regresa: [[("P",True),("Q",True),("R",False)],
            [("P",False),("Q",True),("R",True)],
            [("P",False),("Q",True),("R":r,False)],
            [("P",False),("Q",False),("R",True)],
            [("P",False),("Q",False),("R",False)]] -}          
