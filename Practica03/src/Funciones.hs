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

module Funciones where

myMap :: (a->b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = f x : myMap f xs


myFilter :: (a->Bool) -> [a] -> [a]
myFilter f [] = []
myFilter f (x:xs) = if f x then x:myFilter f xs else myFilter f xs


auxlistas :: [Double] -> Double -> [Double]
auxlistas x y =  myMap (*3) (myMap (+ y) x)


-- 1 Fn que dada una lista de listas le sume un elemento y luego lo multiplica por pi
listas :: [[Double]] -> Double -> [[Double]]
listas x y = myMap (\x -> (auxlistas x y)) x') x'
    where x' = x --cada elemento de la lista

-- 2 Dada una lista de cadenas agrega el prefijo "pro" y sujifo "azo"
cadenas :: [[Char]] -> [[Char]]
cadenas x = myMap (\a -> "pro"++a++"azo") x


-- 3 Dada una lista de listas filtra las que tienen longitud mayor a 4 pero menor a 8
-- Es mayor o menor estricto
filtra :: [[a]] -> [[a]]
filtra x = myFilter (\a -> length a < 8 && length a > 4) x


-- 4 Dada una cadena filtra las que no son vocales, es decir no b,c,d,f,g....,y,z
-- No debe tener la a, e, i, o, u
filtra2 :: [Char] -> [Char]
filtra2 x = myFilter (\a -> a /= 'a' && a /= 'e' && a /= 'i' && a /= 'o' && a /= 'u') x


--------------------------------- Pruebas -------------------------------
-- Prueba de la 1
plistas :: [[Double]]
plistas = listas [[1.0,2.0,3.0], [4.0,5.0,6.0], [7.0,8.0,9.0]] 2
prlistas = listas [[1.0]] 2

aux :: [Double]
aux = auxlistas [1,2,3,4,5,6,7] 8 


-- Prueba de la 2
pcadenas :: [[Char]]
pcadenas = cadenas ["h5la", "no me quiero", "morir"]


-- Prueba de la 3
pfiltra :: [[Char]]
pfiltra = filtra ["hola de", "nuevo", "otra", "vez", "no me quiero morir", "123", "12345", "1234", "12345678", "123456789"]


-- Prueba de la 4
pfiltra2 :: [Char]
pfiltra2 = filtra2 "Murcielago, tambien Murciegalo"


