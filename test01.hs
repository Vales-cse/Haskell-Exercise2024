somma :: Int -> Int
somma 0 = 0
somma n = n + somma (n - 1)

main :: IO ()
main = print (somma 8)