-- Ocurrencias.hs
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