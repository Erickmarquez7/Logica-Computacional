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

data Color = Rojo | Amarillo | Verde | Azul deriving (Show,Eq)

-- Países que pertenecen a la península Balcánica
data Balcanes = Albania
                | Bulgaria
                | BosniaYHerzegovina
                | Kosovo
                | Macedonia
                | Montenegro deriving (Show, Eq)

-- Dos países son adyacentes cuando comparten frontera
type Ady = [(Balcanes,Balcanes)]

adyacencias :: Ady
adyacencias = [(Albania, Montenegro),(Albania ,Kosovo),(Albania,Macedonia),(Bulgaria , Macedonia),
                (BosniaYHerzegovina,Montenegro),(Kosovo,Macedonia),(Kosovo,Montenegro)]

-- x es adyacente a y si (x,y) elem adyacencias ó si (y,x) elem adyacencias

-- Coloración. Esta relaciona a un color, con un país, es una lista de tuplas
type Coloracion = [(Color,Balcanes)]

-- nosotros debemos probar estas xd
colores :: Coloracion
colores = [(Rojo, Montenegro),(Azul, Albania),(Verde,Bulgaria),(Amarillo,BosniaYHerzegovina),
            (Rojo,Kosovo),(Verde, Macedonia)]

-- Regresa si la coloración recibida es buena respecto a la matriz de adyacencias
-- Ejemplo de una buena coloracion: [(Montenegro,Bulgaria)]
-- Ejemplo de una mala coloracion: [(Macedonia,Bulgaria)]

esBuena :: Ady -> Coloracion -> Bool 
esBuena = error  "Falta implentar"
{-esBuena ady colores =
    let 
        col = [[snd x | x <- colores, fst x == Rojo],
               [snd x | x <- colores, fst x == Amarillo],
               [snd x | x <- colores, fst x == Verde],
               [snd x | x <- colores, fst x == Azul]] 
               
    in 
        filter ((> 1) . length) col
               
    verifica :: Ady -> [(Balcanes, Balcanes)] -> Bool
    verifica ady [] = True
    verifica ady (x:xs)
        | elem x ady = False
        | otherwise = verifica ady xs
    -}

-- Calcula todas las coloraciones buenas y completas respecto a la matriz de adyacencias recibida
coloraciones :: Ady -> [Coloracion]
coloraciones = error  "Falta implentar"
-- 
{-coloraciones ady colores =  - }
-- En este metodo, se tiene que usar el resultado obtenido en el anterior
por ejemplo 
coloracions = x+2 esBueno ady xd 
creoooo
-}
