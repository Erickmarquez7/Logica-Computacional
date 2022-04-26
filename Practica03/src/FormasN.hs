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
formaNormalNegativa p = p

formaNormalConjuntiva :: Prop -> Prop
formaNormalConjuntiva p = p

formaNormalDisyuntiva :: Prop -> Prop
formaNormalDisyuntiva p = p




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


interNeg :: Prop -> Prop
interNeg = error "f"
-- p v q
--interNeg (Neg (Var p)) = Var p

normalizar :: Prop -> Prop
normalizar = error "f"
