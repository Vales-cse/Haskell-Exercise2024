-- Definire una funzione swap :: (Int, Int) -> (Int, Int) che, applicata a una coppia di numeri interi, restituisca la coppia con le componenti scambiate.
swap :: (Int, Int) -> (Int, Int)
swap (x,y) = (y,x)

maxn :: (Int, Int) -> (Int, Int)
maxn (x,y) | x >= y      = (y, x)
           | otherwise   = (x, y)

sort :: (Int, Int, Int) -> (Int, Int, Int)
sort (x,y,z) | x > y    = sort(y,x,z)
sort (x,y,z) | y > z    = sort(x,z,y)
--sort t = t
-- Non so l'ultima riga cosa faccia...