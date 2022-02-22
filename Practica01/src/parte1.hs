{-
- Lógica Computacional 2022-2
- Profesor: Francisco Hernández Quiroz
- Ayudante: José Ricardo Desales Santos
- Laboratorio: Emiliano Galeana Araujo
- Integrantes:
- Bernal Márquez Erick        317042522
- Deloya Andrade Ana Valeria  317277582
- Perez Romero Natalia Abigail 318144265
-}
-- Otra forma de verlo 1 | x 0  | x 1
data BinPos = U | Cero BinPos | Uno BinPos deriving (Show)

-- Función que devuelve a su sucesor
succ :: BinPos -> BinPos
succ = error "falta implementar"

-- Función que devuelve su suma
suma :: BinPos -> BinPos -> BinPos
suma = error "falta implementar"

-- Función que devuelve su resta
resta :: BinPos -> BinPos -> BinPos
resta = error "falta implementar"

-- Función que devuelve su producto
producto :: BinPos -> BinPos -> BinPos
producto = error "falta implementar"

-- Función que dado un BinPos devuelve el número natural
binPosToInt :: BinPos -> Int
binPosToInt = error "falta implementar"

-- Función que dado un número natural de tipo Int devuelve su representación bajo el tipo BinPos
intToBinPos :: Int -> BinPos
intToBinPos = error "falta implementar"

dos = Cero U  -- 10
tres = Uno U  -- 11
cinco = Uno (Cero U) -- 101