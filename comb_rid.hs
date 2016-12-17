import Data.Word
import Data.List

-- From wiki.haskell.org
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = (map (x:) (combinations (n-1) xs)) ++ (combinations n xs)

-- Pascal rule: il risultato è necessariamente un intero
binomial n k = product [(n-k+1)..n] `div` product [2..k]

-- binomiale dopo la riduzione
binomialrid n k r = (binomial n k) `div` (binomial k r)


riduzioneignorante k lista = nub $ concat $ map (combinations (k - 1)) $ combinations k lista

-- RIDUZIONE EROS
-- Riduzione che cerca il numero minimo di sestine per cui genero tutte le possibili cinquine


-- RIDUZIONE FEDERICO
-- Riduzione in cui si giocano 10 numeri e si cerca di fare almeno una cinquina indovinando una sestina
-- Idea: Eliminare elementi che N-2 elementi uguali
seuguali x y
    | x == y    = 1
    | otherwise = 0
 
contauguali _ [] = 0
contauguali [] _ = 0
contauguali (x:xs) ys = (sum $ map (seuguali x) ys) + contauguali xs ys

filtro n x y 
    | contauguali x y < n = True 
    | otherwise = False

-- Compila ma per ora non fa quello che voglio ovvero un filtro mappato  :(
--filtraggio _ [] = []
filtraggio _ (x:[]) = [x]
--filtraggio n (x:xs) = x : filter (filtro n x) xs
filtraggio n (x:xs) = filter (filtro n x) xs

-- Funzione che dato una lista di combinazioni, produce il numero ridotto di combinazioni necessarie a ridurre a "n" il numero elementi voluti tali che vi sia almeno una "n" dentro una combinazione indovinata.
-- Nel caso di una lista di terni, e n = 2, trova tutti i necessari terni in modo tale che per ogni terno indovinato da principio, avendo selezionato quei dati numeri, ci sia un ambo sicuro giocando un numero ridotto di terni
gigi :: (Ord a, Num a, Eq t) => a -> [[t]] -> [[t]]
gigi _ [] = []
gigi _ (x:[]) = [x]
gigi _ (x:y:[]) = [x]
gigi n (x:xs)  = [x] ++ (gigi n $ filtraggio n (x:xs))

-- Funzione che uso per fare le prove di quello che faccio
prova :: (Foldable t, Ord a) => t [a] -> [a]
prova = sort . nub . concat
-- Es: prove (combinations 2) [[1,2,3],[1,5,6],[4,5,6]
prove f = prova . map f
nprova :: (Foldable t, Ord a) => t [a] -> Int
nprova = length . prova
--nprove = length . prove


--- SEZIONE SCAZZO INIZIO
-- Esempio dal sito ufficiale del super enalotto che con una partenza di sestine, con una selezione di 10 numeri, ha 14 sestine da giocare per avere almeno una cinquina
scomposizioni = [x*16 | x <- [0..13]]
-- Data la grandezza della combinazione n, la riduzione r, e la lista dei numeri selezionati, prende gli elementi pivot creati dalle scomposizioni
riduzionestandard n r lista = map ((combinations n lista) !!) scomposizioni
sottoriduzionestandard n r lista = prova $ combinations (n - r) $ riduzionestandard n r lista
mappastandard z n r lista = map (filtro z (sottoriduzionestandard n r lista)) (map (combinations (n- r)) (combinations n lista))

noverita [] = True
noverita (x:[]) = x
noverita (x:xs)= x && noverita xs

scopritore n r comb listacomb = noverita $ map (filtro (n - r) comb) listacomb

-- Se una combinazione ridotta con "sottoriduzionestandard" xs è contenuta nelle possibili combinazioni integrali ys, allora da False perché non trova un errore! (in effetti è al contrario del senso logico)
yazini n r (x:[]) ys = scopritore n r x ys
yazini n r (x:xs) ys = (scopritore n r x ys) && (yazini n r xs ys)

-- Da modificare perché se si fa l'and logico basta solo una combinazione che va bene, ovvero che dia False, per essere tutto False
yuzini n r xs (y:[]) = yazini n r xs y
yuzini n r xs (y:ys) = (yazini n r xs y) && (yuzini n r xs ys)


yeezini n r (x:[]) ys = [scopritore n r x ys]
yeezini n r (x:xs) ys = (scopritore n r x ys) : (yeezini n r xs ys)

yoozini n r xs (y:[]) = [yeezini n r xs y]
yoozini n r xs (y:ys) = (yeezini n r xs y) : (yoozini n r xs ys)



pippo = sottoriduzionestandard 6 1 [1..10]
pippe = map (combinations (6- 1)) (combinations 6 [1..10])
--- SEZIONE SCAZZO FINE

nes :: Int
nes = 6
les :: [Word8]
les = [1..10]


combinazioni = combinations nes les

main = do
    print $ binomial (length les) nes
--    print $ combinazioni
