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

suma (Cero x) (Cero y) = Cero (suma x y)
suma (Uno x) (Uno y) = Cero (suma (suma x U) y)
suma (Uno x) (Cero y) = Uno (suma x y)
suma (Cero x) (Uno y) = Uno (suma x y)
 

-- Función que devuelve su resta
resta :: BinPos -> BinPos -> BinPos
resta U U = U --pk no hay cero
 
resta (Cero x) U = Uno (resta x U)-- 110 - 1 = 101  Uno ((resta x U) x)
                -- dos casos 

resta U (Cero x) = U
resta (Uno x) U = Cero x
resta U (Uno x) = U

{- 
1   1   
2   10
3   11
4   100
5   101
6   110
7   1 1 1
8   1 00 0
9   1001
10  1 01 0
11  1 01 1
12  1100
13  1101
14  1 11 0
15  1 11 1
16  1 000 0
-}

resta (Cero x) (Cero y) = Cero (resta x y)
resta (Uno x) (Uno y) = Cero (resta x y)
resta (Uno x) (Cero y) = Uno (resta x y)
resta (Cero x) (Uno y) = U
--resta = error "falta implementar"

{-
restas :: Peano -> Peano -> Peano
restas m Zero = m
restas Zero m = restas m Zero
restas (Succ m) (Succ Zero) = m 
restas n m =  restas (restas n (Succ Zero)) (restas m (Succ Zero))-}

-- Función que devuelve su producto
producto :: BinPos -> BinPos -> BinPos
producto U U  = U

producto U (Cero x) = Cero x    --zzz0 1
producto U (Uno x) = Uno x  -- zzz1 1
producto (Uno x) U = Uno x -- zzz1  1
producto (Cero x) U = Cero x

producto (Cero x) (Uno y) = suma (Cero x) (Cero (producto (Cero x) y))  -- zzz0  zzz1   
producto (Uno x) (Cero y) = suma (Cero y)  (Cero (producto (Cero x) y)) -- zzz1  zzz0
producto (Cero x) (Cero y) = Cero (suma (Cero x) y)
producto (Uno x) (Uno y) = suma x (Cero y)



--producto = error "falta implementar"

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


uno = U
dos = Cero U  -- 10
tres = Uno U  -- 11
cuatro = Cero (Cero U)-- 100
cinco = Uno (Cero U) -- 101
seis = Cero (Uno U) -- 110
siete = Uno (Uno U) -- 111
ocho = Cero (Cero (Cero U))  -- 1000
nueve = Uno (Cero (Cero U)) -- 1001
diez = Cero (Uno (Cero U)) -- 1010

cientoVeinticuatro = Cero(Cero (Uno (Uno (Uno (Uno U)))))