import Data.Word

-- From wiki.haskell.org
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = (map (x:) (combinations (n-1) xs)) ++ (combinations n xs)

-- Pascal rule: il risultato è necessariamente un intero
binomial n k = product [(n-k+1)..n] `div` product [2..k]


nes :: Int
nes = 6
les :: [Word8]
les = [1..90]


combinazioni = combinations nes les

main = do
    print $ binomial (length les) nes
--    print $ combinazioni
