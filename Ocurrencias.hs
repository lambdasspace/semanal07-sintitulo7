-- Ocurrencias.hs
-- Funcion que usa recursion
cuentaElemento :: Int -> [Int] -> Int
cuentaElemento _ [] = 0
cuentaElemento x [y]
    | x == y    = 1
    | otherwise = 0
cuentaElemento x (y:ys)
    | x == y    = 1 + cuentaElemento x ys
    | otherwise = cuentaElemento x ys

ocurrenciasElementos :: [Int] -> [Int] -> [(Int, Int)]
ocurrenciasElementos _ [] = []
ocurrenciasElementos xs (y:ys) = (y, cuentaElemento y xs) : ocurrenciasElementos xs ys


-- Funcion que usa recursion de cola
cuentaElemento2 :: Int -> [Int] -> Int
cuentaElemento2 n xs = cuentaElemento' n xs 0
    where
        cuentaElemento' :: Int -> [Int] -> Int -> Int
        cuentaElemento' _ [] acc = acc
        cuentaElemento' n [x] acc
            | n == x    = acc + 1
            | otherwise = acc
        cuentaElemento' n (x:xs) acc
            | n == x    = cuentaElemento' n xs (acc + 1)
            | otherwise = cuentaElemento' n xs acc 

ocurrenciasElementos2 :: [Int] -> [Int] -> [(Int, Int)]
ocurrenciasElementos2 xs ys = ocurrenciasElementos' xs ys []
    where
        ocurrenciasElementos' :: [Int] -> [Int] -> [(Int, Int)] -> [(Int, Int)]
        ocurrenciasElementos' _ [] acc = acc
        ocurrenciasElementos' xs (y:ys) acc = ocurrenciasElementos' xs ys (acc ++ [(y, cuentaElemento2 y xs)])