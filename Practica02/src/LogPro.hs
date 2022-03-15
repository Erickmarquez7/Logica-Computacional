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

module LogProp where

import Data.List

data Prop =
  Var Name
  | Neg Prop
  | Conj Prop Prop
  | Disy Prop Prop
  | Impl Prop Prop
  | Syss Prop Prop deriving Show

-- Variables.
type Name = String

-- Sustitución.
type Sust = [(Name, Name)]

-- Estado de una variable.
type Estado = (Name, Bool)

-- Solamente muchos estados.
type Estados = [Estado]

-- Un renglon es una lista de variables con su asignacion (True/False)
-- y un resultado.
type Renglon = (Estados, Bool)

-- Una tabla es solo una lista de renglones.
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
  (Conj x y) == (Conj x' y') = (x == x') && (y == y')
  (Disy x y) == (Disy x' y') = (x == x') && (y == y')
  (Impl x y) == (Impl x' y') = (x == x') && (y == y')
  (Syss x y) == (Syss x' y') = (x == x') && (y == y')
  _ == _ = False

--------------------------------------------------------------------------------
--------                            FUNCIONES                           --------
--------------------------------------------------------------------------------

-- | Funcion que saca todos los átomos de una proposicion.
varList :: Prop -> [Name]
varList (Var p) = [p] 
varList (Neg p) = varList p
varList (Conj p q) = (varList p) ++ (varList q)
varList (Disy p q) = (varList p) ++ (varList q)
varList (Impl p q) = (varList p) ++ (varList q)
varList (Syss p q) = (varList p) ++ (varList q) 

-- | Funcion que elimina las implicaciones y dobles implicaciones de
-- una proposicion.
equivalencia :: Prop -> Prop
equivalencia (Var p) = Var p
equivalencia (Neg p) = Neg (equivalencia p)
equivalencia (Conj p q) = Conj (equivalencia p) (equivalencia q)
equivalencia (Disy p q) = Disy (equivalencia p) (equivalencia q)
equivalencia (Impl p q) = (Disy (Neg p) (equivalencia q))
equivalencia (Syss p q) = Conj (Disy (Neg p) (equivalencia q)) (Disy (Neg q) (equivalencia p))
--equivalencia = error "D:"

-- | Funcion que niega una proposicion
negacion :: Prop -> Prop
negacion = error "D:"

-- | Funcion que dada una proposicion y una sustitucion, sustituye las
-- variables que correspondan.
sustituye :: Prop -> Sust -> Prop
sustituye = error "D:"

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
imp = (Impl (Var "P") (Conj (Var "Q") (Neg (Var "R"))))

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
