import Complejos

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

-- funciones auxiliares para el ejercicio 3
divisible :: Int -> Int -> Bool
divisible x y = x `mod` y == 0

divisores :: Int -> [Int]
divisores x = [y | y <- [1..x], divisible x y ]

esPrimo :: Int -> Bool
esPrimo 2 = True
esPrimo n = long(divisores n) == 2

-- calcula los primos dentro del rango [Int] mediante la criba de eratotenes
criba :: [Int] -> [Int]
criba [] = []
criba (n:ns) = n:criba(eliminarMultiplosDe n ns)

-- elimina los multiplos de n que esten en la lista [Int]
eliminarMultiplosDe :: Int -> [Int] -> [Int]
eliminarMultiplosDe n [] = []
eliminarMultiplosDe n (xs:s) | divisible xs n = eliminarMultiplosDe n s
eliminarMultiplosDe n (xs:s) = (xs:eliminarMultiplosDe n s)

-- ejercicio 3
seCumpleGoldbachParaParesMenoresQue :: Int -> Bool
-- mi caso base es 5, porque el menor entero para el que se cumple goldbach es 4
seCumpleGoldbachParaParesMenoresQue 5 = True
-- si no es numero par, entonces paso el proximo numero
seCumpleGoldbachParaParesMenoresQue n | not (divisible n 2) = True && seCumpleGoldbachParaParesMenoresQue (n-1)
-- si n e es par, me fijo si es suma de dos pares y si lo es, me fijo el proximo numero
seCumpleGoldbachParaParesMenoresQue n = (esSuma2Primos n cribaEras) && seCumpleGoldbachParaParesMenoresQue (n-1)
				where cribaEras = criba [2..n]
-- podria preguntar si se cumple directamente para n-2, en vez de n-1, pero por como esta hecho el enunciado
-- me pide para menores que n, y por tanto, mi caso base debe ser 5, y si pusiese n-2, nunca llegaria al caso
-- base y por tanto entraria en un ciclo infinito


-- se fija si n es suma de dos numero que estan en la lista
-- el segundo parametro es una lista de primos
esSuma2Primos :: Int -> [Int] -> Bool
esSuma2Primos n [] = False
esSuma2Primos n [x] = False
esSuma2Primos n (x:xs) | esPrimo(n-x) = True
		       | otherwise = esSuma2Primos n xs

-- ejercicio 4
-- al importar el modulo complejo, se puede hacer:
-- elevarComplejo (C r i) n

-- ejercicio 5

