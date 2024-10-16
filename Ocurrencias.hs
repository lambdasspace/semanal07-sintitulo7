-- Ocurrencias.hs

-- Necesitas importar el módulo Debug.Trace para usar trace
import Debug.Trace

-- Función auxiliar que cuenta las ocurrencias de un elemento en una lista
cuentaElemento :: Int -> [Int] -> Int
cuentaElemento _ [] = 0
cuentaElemento x [y] = 
    let resultado = if x == y then 1 else 0
    in trace ("cuentaElemento " ++ show x ++ " [ " ++ show y ++ " ] = " ++ show resultado) resultado
cuentaElemento x (y:ys) = 
    let resultado = if x == y 
                    then 1 + cuentaElemento x ys 
                    else cuentaElemento x ys
    in trace ("cuentaElemento " ++ show x ++ " (" ++ show (y:ys) ++ ") = " ++ show resultado) resultado

-- Función principal que devuelve una lista de parejas con las ocurrencias de los elementos de la segunda lista en la primera
ocurrenciasElementos :: [Int] -> [Int] -> [(Int, Int)]
ocurrenciasElementos _ [] = []
ocurrenciasElementos xs (y:ys) = 
    let resultado = (y, cuentaElemento y xs) : ocurrenciasElementos xs ys
    in trace ("ocurrenciasElementos (" ++ show xs ++ ") (" ++ show (y:ys) ++ ") = " ++ show resultado) resultado


