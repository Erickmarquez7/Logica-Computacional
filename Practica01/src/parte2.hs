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
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move guards forward" #-}

data Color = Rojo | Amarillo | Verde | Azul deriving (Show,Eq)

-- Países que pertenecen a la península Balcánica
data Balcanes = Albania
                | Bulgaria
                | BosniaYHerzegovina
                | Kosovo
                | Macedonia
                | Montenegro deriving (Show, Eq, Ord)

-- Dos países son adyacentes cuando comparten frontera
type Ady = [(Balcanes,Balcanes)]

adyacencias :: Ady
adyacencias = [(Albania, Montenegro),(Albania ,Kosovo),(Albania,Macedonia),(Bulgaria , Macedonia),
                (BosniaYHerzegovina,Montenegro),(Kosovo,Macedonia),(Kosovo,Montenegro)]

-- x es adyacente a y si (x,y) elem adyacencias ó si (y,x) elem adyacencias

-- Coloración. Esta relaciona a un color, con un país, es una lista de tuplas
type Coloracion = [(Color,Balcanes)]


esBuena :: Ady -> Coloracion -> Bool
esBuena ady col =
    let
        colores = [[snd x | x <- col, fst x == Rojo],
               [snd x | x <- col, fst x == Amarillo],
               [snd x | x <- col, fst x == Verde],
               [snd x | x <- col, fst x == Azul]]

    in
        verifica ady (creaTodasLasTuplas $ filter ((> 1) . length) colores)


--creaTodasLasTuplas :: [Color] -> [Balcanes] -> [(Color,Balcanes)]
creaTodasLasTuplas :: [[b]] -> [(b, b)]
creaTodasLasTuplas [] = []
creaTodasLasTuplas (z: zs) = [(x,y)|x <- z, y <- z] ++ creaTodasLasTuplas zs


verifica :: Ady -> [(Balcanes, Balcanes)] -> Bool
verifica ady [] = True
verifica ady (x:xs)
    | elem x ady = False
    | otherwise = verifica ady xs

colores = [Rojo,Amarillo,Verde,Azul]
balcanes = [Albania,Bulgaria,BosniaYHerzegovina,Kosovo,Macedonia,Montenegro]

-- Calcula todas las coloraciones buenas y completas respecto a la matriz de adyacencias recibida
coloraciones :: Ady -> [Coloracion]
coloraciones ad = filter (esBuena ad) colorcitos
    where
        colorcitos = [[(c1, b1)]
           ++
             [(c2, b2)]
               ++ [(c3, b3)] ++ [(c4, b4)] ++ [(c5, b5)] ++ [(c6, b6)] |
           c1 <- colores,
           c2 <- colores,
           c3 <- colores,
           c4 <- colores,
           c5 <- colores,
           c6 <- colores,
           b1 <- balcanes,
           b2 <- balcanes,
           b1 /= b2,
           b3 <- balcanes,
           b1 /= b3,
           b4 <- balcanes,
           b1 /= b4,
           b5 <- balcanes,
           b1 /= b5,
           b6 <- balcanes,
           b1 /= b6,
           b2 /= b3,
           b2 /= b4,
           b2 /= b5,
           b2 /= b3,
           b3 /= b4,
           b3 /= b5,
           b3 /= b6,
           b4 /= b5,
           b4 /= b6,
           b5 /= b6,
           b1 < b2,
           b1 < b3,
           b1 < b4,
           b1 < b5,
           b1 < b6,
           b2 < b3,
           b2 < b4,
           b2 < b5,
           b2 < b3,
           b3 < b4,
           b3 < b5,
           b3 < b6,
           b4 < b5,
           b4 < b6,
           b5 < b6]


colBuena = [(Rojo, Albania), (Rojo, Bulgaria), (Rojo, BosniaYHerzegovina), (Azul, Kosovo),
            (Amarillo, Macedonia), (Amarillo, Montenegro)]

colMala = [(Verde,Albania), (Verde,Bulgaria), (Verde,BosniaYHerzegovina),
            (Azul,Kosovo), (Amarillo,Macedonia), (Verde,Montenegro)]


-- 
{-coloraciones ady colores =  - }
-- En este metodo, se tiene que usar el resultado obtenido en el anterior
por ejemplo 
coloracions = x+2 esBueno ady xd 
creoooo
-}
