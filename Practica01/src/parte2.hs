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
adyacencias = [(Albania, Montenegro),(Albania ,Kosovo),(Albania,Macedonia),(Bulgaria , Macedonia),(BosniaYHerzegovina,Montenegro),(Kosovo,Macedonia),(Kosovo,Montenegro)]

-- x es adyacente a y si (x,y) elem adyacencias ó si (y,x) elem adyacencias

-- Coloración. Esta relaciona a un color, con un país
type Coloracion = [(Color,Balcanes)]

-- Regresa si la coloración recibida es buena respecto a la matriz de adyacencias
esBuena :: Ady -> Coloracion -> Bool 
esBuena = error "Falta implentar"

-- Calcula todas las coloraciones buenas y completas respecto a la matriz de adyacencias recibida
coloraciones :: Ady -> [Coloracion]
coloraciones = error  "Falta implentar"




