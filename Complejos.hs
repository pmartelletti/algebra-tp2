module Complejos (Complejo(..), elevarComplejo) where

data Complejo = C Float Float deriving (Show)

parteReal, parteIm:: Complejo -> Float
parteReal (C r i) = r
parteIm (C r i) = i


elevarComplejo :: Complejo -> Int -> Complejo
elevarComplejo (C r i) 0 = (C 1 0)
elevarComplejo (C r i) n = multiplicar (C r i) (elevarComplejo (C r i) (n-1))

multiplicar :: Complejo -> Complejo -> Complejo
multiplicar a b = (C (parteReal(a) * parteReal(b) - parteIm(a) * parteIm(b)) (parteReal(a) * parteIm(b) + parteIm(a) * parteReal(b)))

