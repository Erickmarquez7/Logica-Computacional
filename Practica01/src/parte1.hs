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

-- Para ayudar a debuggear: binPosToInt(sucesor(intToBinPos 124))
-- binPosToInt(suma (intToBinPos 124)  (intToBinPos 2)  )
-- Función que devuelve a su sucesor
sucesor :: BinPos -> BinPos
sucesor U = Cero U
sucesor (Cero x) = Uno x
sucesor (Uno x) = Cero (sucesor x)


-- Función que devuelve su suma
suma :: BinPos -> BinPos -> BinPos
suma U  U = Cero U
suma (Cero x) U = Uno x
suma U (Cero x) = Uno x

suma (Uno x) U = Cero (suma U x)
suma U (Uno x) = Cero (suma U x)

suma (Cero x) (Cero y) = suma x y
suma (Uno x) (Uno y) = Cero (suma (suma x U) y)
suma (Uno x) (Cero y) = Uno (suma x y)
suma (Cero x) (Uno y) = Uno (suma x y)
--suma = error "falta implementar"

-- Función que devuelve su resta
resta :: BinPos -> BinPos -> BinPos
resta = error "falta implementar"

-- Función que devuelve su producto
producto :: BinPos -> BinPos -> BinPos
producto = error "falta implementar"

-- Función que dado un BinPos devuelve el número natural
binPosToInt :: BinPos -> Int
binPosToInt x = binPosToIntAux x 0

binPosToIntAux :: BinPos -> Int -> Int
binPosToIntAux U  i = 1*(2^i)
binPosToIntAux (Cero x) i = binPosToIntAux x (i+1)
binPosToIntAux (Uno x) i = 1*(2^i) + binPosToIntAux x (i+1)


-- Función que dado un número natural de tipo Int devuelve su representación bajo el tipo BinPos
intToBinPos :: Int -> BinPos
intToBinPos 1 = U
intToBinPos x = if mod x 2 == 0 then Cero (intToBinPos (div x 2)) else Uno (intToBinPos (div x 2)) 

dos = Cero U  -- 10
tres = Uno U  -- 11
cuatro = Cero (Cero U)-- 100
cinco = Uno (Cero U) -- 101
seis = Cero (Uno U) -- 110