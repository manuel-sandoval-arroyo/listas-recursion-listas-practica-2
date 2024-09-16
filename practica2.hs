--------------- Listas y recursión ---------------

longitud :: [a] -> Int
longitud [] = 0
longitud (_ : xs) = 1 + longitud xs

sumaLista :: Num a => [a] -> a
sumaLista [] = 0
sumaLista (x : xs) = x + sumaLista xs

agregaElemento :: [a] -> a -> Bool -> [a]
agregaElemento [] a cabeza = [a]
agregaElemento xs a cabeza = 
    if cabeza then a : xs 
    else xs ++ [a]

maximoLista :: (Num a, Ord a) => [a] -> a
maximoLista [] = 0
maximoLista [x] = x
maximoLista (x : xs) = max x (maximoLista xs)

indice :: [a] -> Int -> a
indice [] _ = error "Indice fuera de rango"
indice (x : xs) 0 = x
indice (x : xs) n =
    if n >= 0 && n <= longitud xs then indice xs (n-1)
    else error "Indice fuera de rango"


--------------- Listas por comprehensión ---------------

divisores :: Int -> [Int]
divisores 0 = []
divisores n = [x | x <- [1..n], n `mod` x == 0]

conjunto :: Eq a => [a] -> [a]
conjunto [] = []
conjunto (x : xs) = x : conjunto [y | y <- xs, y /= x]

numerosPares :: [Int] -> [Int]
numerosPares [] = []
numerosPares xs = [x | x <- xs, x `mod` 2 == 0]