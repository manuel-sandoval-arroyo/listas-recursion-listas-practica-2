--------------- Listas y recursión ---------------

longitud :: [a] -> Int
longitud [] = 0
longitud (_ : xs) = 1 + longitud xs

sumaLista :: Num a => [a] -> a
sumaLista [] = 0
sumaLista (x : xs) = x + sumaLista xs

agregaElemento :: [a] -> a -> Bool -> [a]
agregaElemento xs a cabeza = 
    if cabeza then a : xs 
    else xs ++ [a]

maximoLista :: (Num a, Ord a) => [a] -> a
maximoLista [] = error "Lista vacía"
maximoLista [x] = x
maximoLista (x : xs) = 
    if x > maximoLista xs 
        then x 
        else maximoLista xs

indice :: [a] -> Int -> a
indice [] n = error "Indice fuera de rango"
indice (x : xs) n =
    if n < 0 then error "Indice fuera de rango"
    else if n == 0 then x
    else indice xs (n - 1)


--------------- Listas por comprehensión ---------------

divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], n `mod` x == 0]

conjunto :: Eq a => [a] -> [a]
conjunto [] = []
conjunto (x : xs) = x : conjunto [y | y <- xs, y /= x]

numerosPares :: [Int] -> [Int]
numerosPares xs = [x | x <- xs, x `mod` 2 == 0]