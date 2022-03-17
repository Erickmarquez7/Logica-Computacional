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
sustituye (Var p) (z:zs) = if (fst z == p) then (Var (snd z)) else (sustituye (Var p) (zs))
sustituye (Neg p) (z:zs) =  Neg (sustituye p (z:zs))
sustituye (Conj p q) (z:zs) = Conj (sustituye p (z:zs)) (sustituye q (z:zs))
sustituye (Disy p q) (z:zs) = Disy (sustituye p (z:zs)) (sustituye q (z:zs))
sustituye (Impl p q) (z:zs) = Impl (sustituye p (z:zs)) (sustituye q (z:zs))
sustituye (Syss p q) (z:zs) = Syss (sustituye p (z:zs)) (sustituye q (z:zs))



-- | Funcion que dada una proposición y estados, evalua la proposicion
-- asignando el estado que corresponda. Si no existe una variable,
-- maneja el error.

-- type Estados = [Estado]
-- type Estado = (Name, T/F)
interp :: Prop -> Estados -> Bool
interp (Var p) [(q,b)] = if p==q then b else error ""-- es no hacer nada pk no coinciden las variables
                                                     -- pero no c como hacerlo en haskell gg
{-x es una tupla (var, p)-}
interp (Neg p) x    = not (interp p x)
interp (Conj p q) x =  interp p x || interp q x
interp (Disy p q) x = interp p x && interp q x
interp (Impl p q) x = not (interp p x) || interp q x
interp (Syss p q) x = (not (interp p x) || interp q x) && (not (interp q x) || interp p x)


-- | Funcion que dada una proposicion, dice True si es tautologia,
-- False en otro caso.
esTautologia :: Prop -> Bool
esTautologia (Var p) = error "creo k necesitamos los valores de las variables xd, le wua preguntar al ayudante "
--esTautologia (Neg p)    = 
--esTautologia (Conj p q) = 
--esTautologia (Disy p q) = 
--esTautologia (Impl p q) = 
--esTautologia (Syss p q) = 
-- idea
-- interpretar -> [t,f,t,f...] = lis
-- if (lis =t) return true
  -- else false

-- | Funcion que dada una proposicion, dice True si es una
-- contradiccion, False en otro caso.
esContradiccion :: Prop -> Bool
esContradiccion (Var p)    = error "creo k necesitamos los valores de las variables xd, le wua preguntar al ayudante "
--esContradiccion (Neg p)    = 
--esContradiccion (Conj p q) = 
--esContradiccion (Disy p q) = 
--esContradiccion (Impl p q) = 
--esContradiccion (Syss p q) = 

-- Hola buen día!
-- Para la practica en las funciones esTautologia y esContradicción
-- No se necesitaría que las variables tengan estado para poder sacar su valor de verdad?
-- O como podemos decir que es contradicción (o tautología) sin tener los valores de las variables?


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

tauto :: Prop
tauto = Conj (Var "P") (Neg (Var "P"))
-- se supone es tauto

contra :: Prop
contra = Disy (Var "P") (Neg (Var "P"))
