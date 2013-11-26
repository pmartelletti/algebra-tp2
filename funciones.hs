--- longitud de una lista
long :: [a] -> Int
long [] = 0
long (x:xs) = 1 + (long xs)

--- suma total de los elementos de una lista de enteros
sumaList :: [Int] -> Int
sumaList [] = 0
sumaList (x:xs) = x + (sumaList xs)

--- elimina el ultimo elemento de una lista de enteros
eliminarUltimo :: [Int] -> [Int]
eliminarUltimo [x] = []
eliminarUltimo (x:xs) = x : eliminarUltimo xs

--- obtengo el ultimo elemento de una lista de enteros
obtenerUltimo :: [Int] -> Int
obtenerUltimo [] = 0
obtenerUltimo x = (x!!(long(x)-1))

--- ejercicio 1
esMultiplo17 :: [Int] -> Bool
esMultiplo17 [x] = False
esMultiplo17 (x:xs) = ((obtenerUltimo xs) * 5 - sumaList (x:(eliminarUltimo xs))) `mod` 17 == 0

potencia:: (Int, Int) -> Int
potencia (b, 0) = 1
potencia (b, n) = b * potencia (b,(n-1))

--- formo un numero en base 10, dados sus elementos en unidades, decenas, etc.
--- ej: formarEnBase10 [2,8,3] devuelve 283
formarEnBase10 :: [Int] -> Int
formarEnBase10 [x] = x
formarEnBase10 (x:xs) = x * potencia(10,long(xs)) + formarEnBase10 xs

--- ejercicio 2
esMultiplo19 :: [Int] -> Bool
esMultiplo19 [x] = False
esMultiplo19 (x:xs) = ((obtenerUltimo xs) * 2 + formarEnBase10 (x:eliminarUltimo xs)) `mod` 19 == 0

-- ejercicio 3
