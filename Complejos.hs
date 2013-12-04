module Complejos (Complejo(..), elevarComplejo ) where

data Complejo = C Float Float deriving (Show)

parteReal, parteIm:: Complejo -> Float
parteReal (C r i) = r
parteIm (C r i) = i

--- eleva un numero complejo a la potencia N, siendo n un numero entero
--- no necesariamente positivo
elevarComplejo :: Complejo -> Int -> Complejo
elevarComplejo (C r i) 0 = (C 1 0)
elevarComplejo (C r i) n | n > 0 = multiplicar (C r i) (elevarComplejo (C r i) (n-1))
elevarComplejo (C r i) n | n < 0 = inversoMulti (elevarComplejo (C r i) absN)
	where absN = abs(n)

-- multiplicacion de dos numeros complejos
multiplicar :: Complejo -> Complejo -> Complejo
multiplicar a b = (C (parteReal(a) * parteReal(b) - parteIm(a) * parteIm(b)) (parteReal(a) * parteIm(b) + parteIm(a) * parteReal(b)))

-- calcula Z^1 siendo Z un numero complejo
inversoMulti :: Complejo -> Complejo
inversoMulti (C r i) = (C (r/sumCuad) (-i/sumCuad))
	where
		sumCuad = r ** 2 + i ** 2
