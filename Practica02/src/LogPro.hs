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
-- varList "el awa es verde " -> ["el awa es verde "]
-- varList (Neg "el awa es verde ") -> VarList "el awa es verde"
-- varList (conj "el awa.." "el sielo") -> varList "awa" ++ varList "sielo" -> awa ++ sielo 
-- 

-- | Funcion que elimina las implicaciones y dobles implicaciones de
-- una proposicion.
equivalencia :: Prop -> Prop
equivalencia (Var p) = Var p
equivalencia (Neg p) = Neg (equivalencia p)
equivalencia (Conj p q) = Conj (equivalencia p) (equivalencia q)
equivalencia (Disy p q) = Disy (equivalencia p) (equivalencia q)
equivalencia (Impl p q) = Disy (Neg p) (equivalencia q)
equivalencia (Syss p q) = Conj (Disy (Neg p) (equivalencia q)) (Disy (Neg q) (equivalencia p))
--equivalencia = error "D:"

-- | Funcion que niega una proposicion
negacion :: Prop -> Prop
negacion (Var p) = Neg (Var p)
negacion (Neg p) = p
negacion (Conj p q) = Conj (negacion p) (negacion q) 
negacion (Disy p q) = Disy (negacion p) (negacion q)
negacion (Impl p q) = Impl (negacion p) (negacion q)
negacion (Syss p q) = Syss (negacion p) (negacion q)


-- | Funcion que dada una proposicion y una sustitucion, sustituye las
-- variables que correspondan.
-- type Sust = [(Name,Name)]
sustituye :: Prop -> Sust -> Prop
sustituye (Var p) [(q,r)] = if p==q then Var r else Var p
sustituye (Neg p) x = sustituye p x
sustituye (Conj p q) x = Conj (sustituye p x) (sustituye q x)
sustituye (Disy p q) x = Disy (sustituye p x) (sustituye q x)
sustituye (Impl p q) x = Impl (sustituye p x) (sustituye q x)
sustituye (Syss p q) x = Syss (sustituye p x) (sustituye q x)
 

-- | Funcion que dada una proposición y estados, evalua la proposicion
-- asignando el estado que corresponda. Si no existe una variable,
-- maneja el error.
interp :: Prop -> Estados -> Bool
interp = error "D:"

-- | Funcion que dada una proposicion, dice True si es tautologia,
-- False en otro caso.
esTautologia :: Prop -> Bool
esTautologia = error "D:"

-- | Funcion que dada una proposicion, dice True si es una
-- contradiccion, False en otro caso.
esContradiccion :: Prop -> Bool
esContradiccion = error "D:"

-- | Funcion que dada una proposicion, dice True si es satisfacible,
-- False en otro caso.
esSatisfacible :: Prop -> Bool
esSatisfacible = error "D:"

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

--------------------------------------------------------------------------------
--------                             EJEMPLOS                           --------
--------------------------------------------------------------------------------

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
