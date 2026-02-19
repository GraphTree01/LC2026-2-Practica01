module Practica01 where

--TIPOS ALGEBRAICOS

--Ejercicio 1
data Shape = Circle Float | --representa el radio
             Square Float | --representa un lado
             Rectangle Float Float| --representa base y altura
             Triangle Float | --representa un lado
             Trapeze Float Float Float --representa base mayor, base menor y altura
  deriving (Show, Eq)

--Funcion que calcula el area de las figuras
area :: Shape -> Float
area (Circle x) = x * x * 3.14159
area (Square x) = x * x
area (Rectangle x y) = x * y
area (Triangle x) = ((sqrt 3) / 4) * x * x
area (Trapeze x y z) = (x + y) * z / 2


--Funcion que calcula el perimetro de las figuras
perimeter :: Shape -> Float
perimeter (Circle x) = 2 * 3.14159 * x
perimeter (Square x) = 4 * x
perimeter (Rectangle x y) = (x + y) * 2
perimeter (Triangle x) = 3 * x
perimeter (Trapeze x y z) = x + y + 2 * lado
  where
    lado = sqrt (z * z + h * h)
    h = (x - y) / 2

--Ejercicio 2 (Les toca arreglar el sinonimo)
type Point = (Float, Float) 

-- Funcion para calcular la distancia entre dos puntos
distance :: Point -> Point -> Float
distance (a, b) (c, d) = sqrt (dx * dx + dy * dy)
  where
    dx = c - a
    dy = d - b

--Funcion para calcular la distancia de un punto al origen
from0 :: Point -> Float
from0 (x, y) = sqrt (x * x + y * y)

--Ejercicio 3
data Haskellium = Haskellium {name :: String,
                               lastName1 :: String,
                               lastName2 :: String,
                               location :: Point,
                               houseShape :: Shape} deriving Show

--Funcion para regresar el hijo de dos Haskelliums dado su nombre
son :: Haskellium -> Haskellium -> String -> Haskellium
son p1 p2 n = Haskellium {name = n,
  lastName1 = lastName1 p1,
  lastName2 = lastName1 p2,
  location = location p1,
  houseShape = houseShape p1}

--Funcion para calcular las unidades para construir la casa de un Haskellium
houseCost :: Haskellium -> Float
houseCost p = perimeter (houseShape p) * altura + area (houseShape p)
  where altura = 2.5

--Funcion para calcular el tiempo que le toma a un Haskellium para llegar a su trabajo
timeToWork :: Haskellium -> Float
timeToWork p
  | distancia > 299 =  distancia / 70
  | otherwise = distancia / 30
  where
    distancia = from0 (location p)

--LISTAS Y FUNCIONES
--Ejercicio 1
palindromo :: String -> Bool
palindromo xs = xs == reversa xs

--Función Auxiliar para el Ejercicio 1. Regresa la inversa de una cadena
reversa :: String -> String
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]

--Ejercicio 2
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ x [] = x
myFoldr f v (x:xs) = f x (myFoldr f v xs)
                      
--Ejercicio 3
conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia [] = [[]]
conjuntoPotencia (x:xs) = conjuntoPotencia xs ++ [x:ys | ys <- conjuntoPotencia xs]
--ARBOLES

--Implementacion

data OneTwoTree a = Void |
                    Node a (OneTwoTree a) |
                    Branch a (OneTwoTree a) (OneTwoTree a)
  deriving (Show)

--Ejercicio 2
suma :: OneTwoTree Int -> Int
suma Void = 0
suma (Node x t) = x + suma t
suma (Branch x l r) = x + suma l + suma r
