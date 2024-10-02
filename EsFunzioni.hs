import System.Posix (otherExecuteMode)
area1 :: Float -> Float -> Float
area1 b h = (b * h)/2

area3 :: Float -> Float -> Float -> Float
area3 a b h = ((a + b)*h)/2

isEven :: Integer -> Bool
isEven x = x `mod` 2 == 0

p :: Float -> Float
p x = x^2 + 2*x + 1

fibo :: Int -> Int
fibo x | x == 0 || x == 1    = x
       | otherwise = fibo (x - 1) + fibo (x-2)

-- Definire la funzione pow2 :: Int -> Int che, applicata a un numero intero non negativo n, calcoli 2n senza usare gli operatori ˆ e ** di Haskell.
pow2 :: Int -> Int
pow2 x | x < 0       = -1
       | x == 0      = 1
       | x == 1      = 2
       | otherwise   = 2 * pow2(x-1)