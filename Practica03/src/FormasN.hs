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

module FormasN where

import LogPro


formaNormalNegativa :: Prop -> Prop
formaNormalNegativa p = eliminaNeg(equivalencia p)

formaNormalConjuntiva :: Prop -> Prop
formaNormalConjuntiva p = interDisy (formaNormalNegativa p)

formaNormalDisyuntiva :: Prop -> Prop
formaNormalDisyuntiva p = interConj (formaNormalNegativa p)


------------------------------ Auxiliares -------------------------
-- Suponemos que lo que nos pasen esta en FNN
-- FNN -> ¬p v q, ¬p ^ ¬q
-- (P v Q) ^ R   ->   (P ^ R) v (Q ^ R)      "Es como distribuir xd" 
interConj :: Prop -> Prop
interConj (Conj (Disy p q) r) = interConj 
                                (Disy 
                                (Conj (interConj p) 
                                (interConj r)) 
                                (Conj (interConj q) 
                                (interConj r)))

-- P ^ (Q v R)   ->   (P ^ Q) v (P ^ R)      "Es como distribuir xd" 
interConj (Conj p (Disy q r)) = interConj 
                                (Disy 
                                (Conj (interConj p) 
                                (interConj q)) 
                                (Conj (interConj p) 
                                (interConj r)))
-- P v Q
interConj (Disy p q) = Disy (interConj p) (interConj q)
interConj p = p


-- Lo mismo pero con la disyuncion
interDisy :: Prop -> Prop
-- (P ^ Q) v R   ->   (P v R) ^ (Q v R)      "Es como distribuir xd" 
interDisy (Disy (Conj p q) r) = interDisy
                                (Conj 
                                (Disy (interDisy p) 
                                (interDisy r)) 
                                (Disy (interDisy q) 
                                (interDisy r)))
-- P v (Q ^ R)   ->   (P v Q) ^ (P v R)      "Es como distribuir xd" 
interDisy (Disy p (Conj q r)) = interDisy
                                (Conj 
                                (Disy (interDisy p) 
                                (interDisy q)) 
                                (Disy (interDisy p) 
                                (interDisy r)))
-- P v Q
interDisy (Conj p q) = Conj (interDisy p) (interDisy q)
interDisy p = p



----------------------------- Ejemplos -----------------------------

-- Ejemplos de Forma Normal Disyuntiva
ejemplo1Disy::Prop
-- ¬(p -> q) v ¬(q -> p) 
ejemplo1Disy =  Disy ( Neg (Impl (Var "P") (Var "Q"))) (Neg (Impl (Var "Q") (Var "P")))

-- ¬(p -> q) v ¬(q -> p) 
-- ¬(¬p v q) v ¬(¬q v p) 
-- (¬¬p v ¬q) v (¬¬q v ¬p) 
-- (p ^ ¬q) v (q v ¬p) 
-- Disy (Conj (Var "P") (Neg (Var "Q"))) (Conj (Var "Q") (Neg (Var "P")))
ejemplo1Disy' = formaNormalDisyuntiva ejemplo1Disy

-- p v (¬q -> p) 
ejemplo2Disy =  Disy (Var "P") (Impl (Neg (Var "Q")) (Var "P"))
ejemplo2Disy' = formaNormalDisyuntiva ejemplo2Disy
-- p v (¬q -> p) 
-- p v (¬¬q v p)
--  p v (q v p)
-- Disy (Var "P") (Disy (Var "Q") (Var "P"))


--------------------------------------------------------------------

-- Ejemplos de Forma Normal Conjuntiva

ejemplo1Conj :: Prop
-- ¬(p ^ q) ^ ¬(r v q)
ejemplo1Conj = Conj (Neg(Conj((Var "P")) ((Var "Q")))) ((Neg(Disy((Var "R")) ((Var "Q")))))

-- ¬(p ^ q) ^ ¬(r v q)
-- (¬p ^ ¬q) ^ (¬r ^ ¬q)
-- Conj (Disy (Neg (Var "P")) (Neg (Var "Q"))) (Conj (Neg (Var "R")) (Neg (Var "Q")))
ejemplo1Conj' = formaNormalConjuntiva ejemplo1Conj


-- ¬p ^ (p -> q)
ejemplo2Conj = Conj (Neg (Var "P")) (Impl (Var "P") (Var "Q"))

-- ¬(p ^ q) ^ ¬(r v q)
-- (¬p v ¬q) ^ (¬r ^ ¬q)
-- ¬p ^ (¬p v q)
-- Conj (Neg (Var "P")) (Disy (Neg (Var "P")) (Var "Q"))
ejemplo2Conj' = formaNormalConjuntiva ejemplo2Conj


--------------------------------------------------------------------

-- Ejemplos de Forma Normal Negativa

ejemplo1Neg :: Prop
-- ¬(p v (r ^ q))
ejemplo1Neg = Neg(Disy (Var "P") (Conj (Var "R") (Var "Q")))

-- (¬p ^ ¬(r ^ q))
-- (¬p ^ (¬r v ¬q))
-- Conj (Neg (Var "P")) (Disy (Neg (Var "R")) (Neg (Var "Q")))
ejemplo1Neg' = formaNormalNegativa ejemplo1Neg


ejemplo2Neg :: Prop
-- ¬((p ^ q) => (r ^ s))
ejemplo2Neg = Neg(Impl (Conj (Var "P")(Var "Q"))  (Conj (Var "R")(Var "S")))


-- (¬(p ^ q) v (r ^ s))
-- ((¬p v ¬q) v (r ^ s))
-- ((p ^ q) ^ (¬r v ¬s))
-- Conj (Conj (Var "P") (Var "Q")) (Disy (Neg (Var "R")) (Neg (Var "S")))
ejemplo2Neg' = formaNormalNegativa ejemplo2Neg










