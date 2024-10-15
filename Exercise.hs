-- In un file Triangolo.hs dichiarare le quantità base, altezza e ipotenusa di un triangolo rettangolo, fissando i cateti a piacere.
catMin = 13.3

catMag = 15.6

ipot :: Float
ipot = sqrt(catMin^2 + catMag^2)

perimetro :: Float
perimetro = catMin + catMag + ipot

area :: Float
area = (catMin * catMag)/2

altezza :: Float
altezza = 2*area / ipot

calcMeta :: Float -> Float -> Float
calcMeta x y = (x + y)/2

-- roba a caso delle funzioni
p :: Float -> Float
p x = x^2 + 2*x + 1

isEven :: Int -> Bool
isEven x = x`mod`2 == 0

fact :: Int -> Int
fact x  | x == 0        = 1
        | otherwise     = x * fact(x-1)

-- Definire la funzione pow2 :: Int -> Int che, applicata a un numero intero non negativo n, calcoli 2^n senza usare gli operatori ˆ e ** di Haskell.
pow2 :: Int -> Int
pow2 0 = 1
pow2 n = 2 * pow2 (n - 1)

-- powerOfTwo n == True sse n è una potenza di 2
powerOfTwo :: Int -> Bool
powerOfTwo x    |   x == 1                          = True
                |   (x `mod` 2 == 1) && x /= 1      = False
                |   x > 1                           = powerOfTwo(x`div`2)
                |   otherwise                       = False


sort :: (Int, Int, Int) -> (Int, Int, Int)
sort (x,y,z)    | x > y     = sort(y,x,z)
                | y > z     = sort(x,z,y)
                | otherwise = (x,y,z)

-- Definire la funzione somma che calcola la somma degli elementi di liste di numeri interi
sommaLista :: [Int] -> Int
sommaLista (x : xs)      = x + sommaLista(xs)
sommaLista []            = 0

-- Definire una funzione che, applicata a una lista di coppie di numeri interi, produca la lista dei prodotti degli elementi nelle coppie
prodotti :: [(Int,Int)] -> Int
prodotti []             = 1
prodotti ((x,y) : xs)   = (x * y) * prodotti(xs)

-- Definire una funzione che, applicata a una lista di numeri interi, produca la lista dei numeri interi pari presenti nella lista
listaPari :: [Int] -> [Int]
listaPari []                                = []
listaPari (x : xs)  | x `mod` 2 == 0        = [x] ++ listaPari xs
                    | otherwise             = listaPari xs

-- RIPRENDERE DA ESERCIZI SU LISTE
-- Usando l’operatore ++ definire la funzione inversa :: [Int] -> [Int] che inverte l’ordine degli elementi di una lista di numeri interi
inversa :: [Int] -> [Int]
inversa []              = []
inversa (x : xs)        = inversa xs ++ [x]

-- Definire la funzione negazioni :: [Bool] -> [Bool] che nega una lista di valori booleani
negazioni :: [Bool] -> [Bool]
negazioni []                            = []
negazioni (x : xs)      | x == True     = [False] ++ negazioni xs
                        | x == False    = [True] ++ negazioni xs

-- Definire la funzione prods :: [Int] -> Int che calcola il prodotto degli elementi di una lista
prods :: [Int] -> Int
prods []        = 0
prods [x]       = x
prods (x : xs)  = x * prods xs

-- Definire la funzione dispari :: [Int] -> [Int] che, applicata a una lista di numeri interi xs, produca la lista degli elementi dispari di xs
dispari :: [Int] -> [Int]
dispari []                              = []
dispari (x : xs)  | x `mod` 2 == 0      = dispari xs
                  | otherwise           = x : dispari xs

-- Definire la funzione parziale ultimo :: [Int] -> Int per ottenere l’ultimo elemento di una lista, se presente
ultimo :: [Int] -> Int
ultimo [x]      = x
ultimo (x : xs) = ultimo xs

concatRicorsivo :: [a] -> [a] -> [a]
concatRicorsivo [] ys          = ys
concatRicorsivo (x : xs) ys   = x : concatRicorsivo xs ys

concatLists :: [[a]] -> [a]
concatLists []          = []
concatLists (x : xs)    = x ++ concatLists xs

-- Take the first `n` elements of a list.
primiN :: Int -> [a] -> [a]
primiN n []             = []
primiN 0 (x : xs)       = []
primiN n (x : xs)       = x : primiN (n-1) xs

-- `zip` takes two lists and returns a list of corresponding pairs. If one list is shorter, the excess elements of the longer list are discarded.
zipTest :: [a] -> [b] -> [(a, b)]
zipTest [] _                    = []
zipTest _ []                    = []
zipTest (x:xs) (y:ys)           = (x,y) : zipTest xs ys


-- Definire una funzione Haskell che, applicata a una lista xs, calcoli il numero di inversioni di xs, ovvero il numero di elementi di 
-- xs immediatamente seguiti da un elemento più piccolo.
inversioni :: Ord a => [a] -> Int
inversioni []                           = 0
inversioni [_]                          = 0
inversioni (x:y:xs)     | y < x         = 1 + inversioni (y:xs)
                        | otherwise     = inversioni (y:xs)

-- Definire una funzione Haskell che, applicata a una lista xs, restituisca la sotto-lista contenente tutti e soli gli elementi di xs in posizione pari,
-- nello stesso ordine in cui compaiono in xs e assumendo che il primo elemento della lista si trovi in posizione 0.
elementPari :: [a] -> [a]
elementPari []                  = []
elementPari [x]                 = [x]
elementPari (x:y:xs)            = x : elementPari xs

-- Rimuovere i duplicati da una lista
rimuoviDup :: Eq a => [a] -> [a]
rimuoviDup []                   = []
rimuoviDup (x : xs)             = x : rimuoviDup (rimuoviTutti x xs)
        where
                rimuoviTutti :: Eq a => a-> [a] -> [a]
                rimuoviTutti _ []       = []
                rimuoviTutti y (z:zs)   | y == z        = rimuoviTutti y zs
                                        | otherwise     = z : rimuoviTutti y zs

-- Invertire una lista
invertiLista :: [a] -> [a]
invertiLista []                 = []
invertiLista [x]                = [x]
invertiLista (x : xs)           = invertiLista xs ++ [x]

-- Realizzare una funzione con tipo Eq a => [a] -> Bool che, applicata a una lista, determina se la lista contiene elementi duplicati.
elementDupl :: Eq a => [a] -> Bool
elementDupl []                                  = False
elementDupl [x]                                 = False
elementDupl (x:xs)      | checkDupl x xs        = True
                        | otherwise             = elementDupl xs
        where
                checkDupl :: Eq a => a -> [a] -> Bool
                checkDupl _ []                  = False 
                checkDupl y (z:zs) | y == z     = True
                                   | otherwise  = checkDupl y zs

-- Scrivi una funzione ricorsiva palindromo :: Eq a => [a] -> Bool che verifica se una lista è un palindromo.
isPolindromo :: Eq a => [a] -> Bool
isPolindromo []                 = True
isPolindromo [x]                = True
isPolindromo x                  = isEqual x (invertiLista x)
        where
                isEqual :: Eq a => [a] -> [a] -> Bool
                isEqual [] []                           = True
                isEqual _ []                            = False
                isEqual [] _                            = False
                isEqual (y:ys) (z:zs)   | y == z        = isEqual ys zs
                                        | otherwise     = False

-- Scrivi una funzione ricorsiva mappa :: (a -> b) -> [a] -> [b] che applica una funzione a ogni elemento di una lista e restituisce la lista dei risultati.
mappa :: (a -> b) -> [a] -> [b]
mappa _ []              = []
mappa x (z:zs)          = x z : mappa x zs

-- Scrivi una funzione ricorsiva concatenaListe :: [a] -> [a] -> [a] che concatena due liste.
concatenaListe :: [a] -> [a] -> [a]
concatenaListe [] []            = []
concatenaListe [] ys            = ys
concatenaListe (x:xs) y         = x : concatenaListe xs y

-- Scrivi una funzione ricorsiva trovaMassimo :: Ord a => [a] -> a che trova il massimo elemento in una lista non vuota.
trovaMassimo :: Ord a => [a] -> a
trovaMassimo [x]                        = x
trovaMassimo (x:y:ys)   | x >= y        = trovaMassimo (x:ys)
                        | otherwise     = trovaMassimo (y:ys)

-- Raddoppia gli elementi di una lista usando map
raddoppia :: Num a => [a] -> [a]
raddoppia []            = []
raddoppia xs            = map (*2) xs 

-- Prodotto di una lista usando foldr
prodotto :: Num a => [a] -> a
prodotto []     = 0
prodotto xs     = foldr (*) 1 xs

-- Concatenazione di stringhe usando foldr
concatStringhe :: [String] -> String
concatStringhe []       = ""
concatStringhe xs       = foldr (++) "" xs

-- Filtra numeri maggiori di un certo valore usando filter
filterNumber :: Ord a => [a] -> a -> [a]
filterNumber [] _       = []
filterNumber xs y       = filter (>y) xs

-- Filtra le stringhe con lunghezza maggiore di un certo valore x usando filter
filterString :: Int -> [String] -> [String]
filterString _ []       = []
filterString y xs       = filter (\x -> length x > y) xs

-- Calcolare la media degli elementi pari di una lista di numeri interi senza usare la ricorsione
mediaPari :: (Integral a) => [a] -> a
mediaPari xs = sumPari `div` fromIntegral numPari
        where
                listPari = filter even xs
                sumPari = foldr (+) 0 listPari
                numPari = length listPari

-- Calcolare la lunghezza di una lista (senza usare length).
lunghezzaLista :: [a] -> Int
lunghezzaLista xs       = foldr (\_ y -> y + 1) 0 xs

-- Verificare se una lista è ordinata.
listaOrdinata :: Ord a => [a] -> Bool
listaOrdinata (x:xs)    = foldr (\(x,y) acc -> x < y && acc) True (zip (x:xs) xs)

-- Scrivi una funzione che, data una lista di stringhe, restituisca una lista contenente la lunghezza di ogni stringa usando map.
lunghezzaStringhe :: [String] -> [Int]
lunghezzaStringhe xs    = map (\x -> length x) xs

-- Calcolare lo scarto quadratico medio di una lista
scartoQuad :: Floating a => [a] -> a
scartoQuad xs   = sqrt (sum (map(\x -> (x - mu)^2) xs) / fromIntegral n)
        where
                n = length xs
                mu = sum xs / fromIntegral n

-- Definire una funzione Haskell che, applicata a una lista xs, calcoli il numero di inversioni di xs, ovvero il numero di elementi di xs immediatamente
-- seguiti da un elemento piu piccolo.
inversioniRicorsione :: Ord a => [a] -> Int
inversioniRicorsione []                         = 0
inversioniRicorsione [x]                        = 0
inversioniRicorsione (x:y:xs) | y < x           = 1 + inversioniRicorsione (y:xs)
                              | otherwise       = inversioniRicorsione (y:xs)

inversioniFunzioni :: Ord a => [a] -> Int
inversioniFunzioni xs = length(filter (\(x,y) -> x > y) (zip xs (tail xs)))

inversioniLista :: Ord a => [a] -> [a]
inversioniLista xs = map fst (filter (\(x,y) -> x > y) (zip xs (tail xs)))

-- Definire una funzione Haskell che, applicata a una lista xs, restituisca la sotto-lista contenente tutti e soli gli elementi di xs in posizione pari,
-- nello stesso ordine in cui compaiono in xs e assumendo che il primo elemento della lista si trovi in posizione 0.
posizioniPari :: [a] -> [a]
posizioniPari xs = map fst(filter (\(_,i) -> even i) (zip xs [0..]))

findMassimo :: Ord a => [a] -> a
findMassimo (x:xs) = foldl (\y acc -> if y > acc then y else acc) x xs

duplicati :: Eq a => [a] -> Bool
duplicati xs = any duplicato xs
  where
    duplicato x = length (filter (== x) xs) > 1

listaNonOrdinata :: Ord a => [a] -> Bool
listaNonOrdinata xs = any (\(y,z) -> y >= z) (zip xs (tail xs))

rimuoviDuplicati :: Eq a => [a] -> [a]
rimuoviDuplicati xs = foldr (\y acc -> if y `elem` acc then acc else y : acc) [] xs

rimuoviDuplicatiRic :: Eq a => [a] -> [a]
rimuoviDuplicatiRic []                          = []
rimuoviDuplicatiRic [x]                         = [x]
rimuoviDuplicatiRic (x:xs) | contiene x xs      = rimuoviDuplicatiRic xs
                           | otherwise          = x : rimuoviDuplicatiRic xs
        where
                contiene :: Eq a => a -> [a] -> Bool
                contiene _ []                   = False
                contiene y (z:zs) | y == z      = True
                                  | otherwise   = contiene y zs

-- Esercizio 6: Contare quanti elementi soddisfano una condizione
contaCondizione :: (a -> Bool) -> [a] -> Int
contaCondizione cond xs = length(filter (cond) xs)

-- Esercizio 4: Controlla se tutti gli elementi sono positivi
tuttiPositivi :: (Num a, Ord a) => [a] -> Bool
tuttiPositivi = all (>=0)

-- 3. Calcolare il prodotto degli elementi non nulli di una lista di numeri.
prodLista :: (Num a, Eq a) => [a] -> a
prodLista xs = foldr (*) 1 (filter (/= 0) xs)

prodListaRic :: (Num a, Eq a) => [a] -> a
prodListaRic []                 = 1
prodListaRic (x:xs) | x /= 0    = x * prodListaRic xs
                    | otherwise = prodListaRic xs

-- Esercizio 5: Somma dei quadrati
sommaQuadrati :: [Int] -> Int
sommaQuadrati xs = sum (map (^2) xs)

sommaQuadratiV2 :: [Int] -> Int
sommaQuadratiV2 xs = foldr (\x acc -> x^2 + acc) 0 xs

sommaQuadratiRic :: [Int] -> Int
sommaQuadratiRic []             = 0
sommaQuadratiRic (x:xs)         = (x^2) + sommaQuadratiRic xs

-- Definire una funzione Haskell che, applicata a una lista di stringhe xs, concateni tutte le stringhe immediatamente seguite da una stringa di uguale lunghezza. 
-- Ad esempio, tale funzione applicata alla lista ["casa", "pari", "di", "un", "tre"] deve produrre la stringa "casadi".
paroleFunz :: [String] -> String
paroleFunz xs = foldl (++) "" (
        map (fst) (filter (\(y,z) -> length y == length z) (zip xs (tail xs)))
        )

parole :: [String] -> String
parole []                                               = ""
parole [_]                                              = ""
parole (x : y : xs) | myLenght x == myLenght y          = x ++ parole (y:xs)
                    | otherwise                         = parole (y:xs)
        where
                myLenght :: String -> Int
                myLenght []             = 0
                myLenght (x:xs)         = 1 + myLenght xs

-- Implementa una funzione triangolare che calcola il n-esimo numero triangolare.
triangolare :: Int -> Int
triangolare x = x*(x+1) `div` 2

-- Calcolo della Media Armonica
armonica :: [Float] -> Float
armonica xs = fromIntegral (length xs) / sum (map (\x -> 1/x) xs)

-- Calcolo della Media Geometrica
mediaGeom :: [Double] -> Double
mediaGeom xs | null xs          = 0
             | any (<= 0) xs    = error "Ups :)"
             | otherwise        = (foldr (*) 1 xs)**(1 / fromIntegral (length xs))

-- Esercizio: Calcolare la Media Ponderata
mediaPond :: (Floating a, Ord a) => [a] -> [a] -> a
mediaPond xs ws | null xs || null ws            = 0
                | length xs /= length ws        = error "Nope, give me two list with the same length!"
                | otherwise                     = (foldr (\(x,y) acc -> x*y + acc) 0 (zip xs ws)) / (sum ws)

-- Esercizio: Calcolare la Media Quadratica
mediaQuad :: (Floating a, Ord a) => [a] -> a
mediaQuad xs = sqrt ((sum (map (^2) xs)) / fromIntegral (length xs)) 

-- Esercizio 2: Conta le occorrenze di un elemento
numOccorrenzeRic :: Eq a => a -> [a] -> Int
numOccorrenzeRic _ []                   = 0
numOccorrenzeRic y (x:xs) | y == x      = 1 + numOccorrenzeRic y xs
                          | otherwise   = numOccorrenzeRic y xs

numOccorrenzeFun :: Eq a => a -> [a] -> Int
numOccorrenzeFun y xs = length (filter (==y) xs)

-- Esercizio 7: Somma degli elementi nelle posizioni pari
sommaPariRic :: [Int] -> Int
sommaPariRic []                 = 0
sommaPariRic [x]                = x
sommaPariRic (x:y:xs)           = x + sommaPariRic xs

sommaPariFun :: [Int] -> Int
sommaPariFun xs = sum (map fst (filter (\(x,y) -> even y) (zip xs [0,1..])))

-- Esercizio 9: Invertire una lista
invertiListaRic :: [a] -> [a]
invertiListaRic []              = []
invertiListaRic [x]             = [x]
invertiListaRic (x:xs)          = invertiListaRic xs ++ [x]

invertiListaFun :: [a] -> [a]
invertiListaFun xs = foldl (\x acc -> acc : x) [] xs

-- Esercizio 8: Verificare se tutti gli elementi di una lista sono maggiori di un valore
maggioreDiRic :: (Num a, Eq a, Ord a) => a-> [a] -> Bool
maggioreDiRic _ []                      = True
maggioreDiRic y (x:xs) | y > x          = False
                       | otherwise      = maggioreDiRic y xs

maggioreDiFun :: (Num a, Eq a, Ord a) => a-> [a] -> Bool
maggioreDiFun y xs = all (>=y) xs