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
riduzionefederico :: (Ord a, Num a, Eq t) => a -> [[t]] -> [[t]]
riduzionefederico _ [] = []
riduzionefederico _ (x:[]) = [x]
riduzionefederico _ (x:y:[]) = [x]
riduzionefederico n (x:xs)  = [x] ++ (riduzionefederico n $ filtraggio n (x:xs))

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
--- FINE SCAZZO



--- SEZIONE DETENZIONE CORRETTEZZA ALGORITMICA
-- Le funzioni seguenti servono ad accertare che la riduzione che andiamo ad usare realmente funzioni. 
-- Ovvero per ogni combinazione giocata essa ha almeno una sottocombinazione valida di ogni possibile combinazione giocabile per quel dato insieme di numeri di partenza


noverita [] = True
noverita (x:[]) = x
noverita (x:xs)= x && noverita xs

-- Restituisce False solo se ogni sottocombinazione listacomb, ha un elemento generabile da comb
scopritore n r comb listacomb = noverita $ map (filtro (n - r) comb) listacomb

-- Restituisce False solo se ogni sottocombinazione ys, ha un elemento generabile da tutti i vari xs
yazini n r (x:[]) ys = scopritore n r x ys
yazini n r (x:xs) ys = (scopritore n r x ys) && (yazini n r xs ys)

-- Restituisce False se la riduzione funziona
yuzini n r xs (y:[]) = yazini n r xs y
yuzini n r xs (y:ys) = (yazini n r xs y) || (yuzini n r xs ys)

-- Restituisce True se la riduzione funziona, in modo che con la verita sia più user friendly!
yizini n r xs ys = not $ yuzini n r xs ys


-- Funzionalmente simile a yazini, crea la lista dei booleani per ogni lista di sottocombinazioni generata da una combinazione ipotetica
yaaazini n r (x:[]) ys = [scopritore n r x ys]
yaaazini n r (x:xs) ys = (scopritore n r x ys) : (yaaazini n r xs ys)
-- Funzionalmente simile a yuzini, crea una lista di liste di booleani, in cui i False rappresentano che un elemento è generato e quindi una lista con almeno un False è buona! 
yuuuzini n r xs (y:[]) = [yaaazini n r xs y]
yuuuzini n r xs (y:ys) = (yaaazini n r xs y) : (yuuuzini n r xs ys)

-- Funzionalmente identico a yizini, restituisce True se la riduzione funziona, in modo che con la verita sia più user friendly!
yiiizini n r xs ys = discriminante $ yuuuzini n r xs ys


nand = not . and

-- Opera in modo tale: restituisce True, solo se ogni lista debba contentere almeno un elemento False
discriminante :: [[Bool]] -> Bool
discriminante [] = False
discriminante (x:[]) = nand x
discriminante (x:xs) = nand x && discriminante xs

--- FINE SEZIONE DETENZIONE


nes :: Int
nes = 6
les :: [Word8]
les = [1..15]

combinazioni = combinations nes les

combridotti = riduzionefederico nes $ combinazioni

combcombinazioni = map (combinations (nes - 1)) combinazioni

--prova1 = yizini nes 1 combridotti combcombinazioni
--prova2 = yiiizini nes 1 combridotti combcombinazioni

main = do
    print $ binomial (length les) nes
    --print $ yizini nes 1 combridotti combcombinazioni
    print $ yiiizini nes 1 combridotti combcombinazioni
