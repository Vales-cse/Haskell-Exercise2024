
lung :: [Int] -> Int
lung [] = 0
lung (x : xs) = 1 + lung xs


somma :: [Float] -> Float
somma xs = foldr (+) 0 xs


-- Definire una funzione che, applicata a una lista di numeri interi, produca la lista dei numeri interi pari presenti nella lista
listPari :: [Int] -> [Int]
listPari []                 = []
listPari (x : xs) | pari x  = x : listPari xs   -- potevo usare la funzione "even"
listPari (_ : xs)           = listPari xs

pari :: Int -> Bool
pari x | x `mod` 2 == 0     = True  
       | otherwise          = False

-- Definire una funzione chiudi che, applicata a due liste xs e ys, produca la lista degli elementi corrispondenti di xs e ys
chiudi :: [Int] -> [Int] -> [(Int, Int)]
chiudi _    []              = []
chiudi []   _               = []
chiudi (x : xs) (y : ys)    = (x, y) : chiudi xs ys

-- Usando l’operatore ++ definire la funzione inversa :: [Int] -> [Int] che inverte l’ordine degli elementi di una lista di numeri interi
inversa :: [Int] -> [Int]
inversa []                  = []
inversa (x : xs)            = inversa xs ++ [x]
-- Ora senza ++
--inversa2 :: [Int] -> [int]
--inversa2 []                 = []

-- Definire la funzione negazioni :: [Bool] -> [Bool] che nega una lista di valori booleani
negazioni :: [Bool] -> [Bool]
negazioni []                       = []
negazioni (x : xs)  |  x           = False : negazioni xs    
                    |  not x       = True : negazioni xs

-- Definire la funzione prods :: [Int] -> Int che calcola il prodotto degli elementi di una lista
prods :: [Int] -> Int
prods []             = 0
prods [x]            = x
prods (x : xs)       = x * prods xs
--prods (x : xs)       = foldr (*) x xs

-- Definire la funzione dispari :: [Int] -> [Int] che, applicata a una lista di numeri interi xs, produca la lista degli elementi dispari di xs
dispari :: [Int] -> [Int]
dispari []                                = []
dispari (x : xs)  |  x `mod` 2 /= 0       = x : dispari xs
                  | otherwise             = dispari xs

--Definire la funzione parziale ultimo :: [Int] -> Int per ottenere l’ultimo elemento di una lista, se presente
-- posso ignorare la scrittura dei tipi, haskell lo inferisce automaticamente!
--ultimo :: [Int] -> Int
ultimo []                                 = 0
ultimo (x : xs)  |  not (lastEl xs)       = ultimo xs
                 |  lastEl xs             = x
       where
              lastEl :: [Int] -> Bool
              lastEl xs  |  xs == []      = True
                         |  xs /= []      = False