import Data.Map

count :: (Ord a, Num b) => [a] -> Map a b
count [] = empty
count (x:xs) = inc x $ count xs where
    inc :: (Ord a, Num b) => a -> Map a b -> Map a b
    inc k m = insert k (get k m + 1) m

get :: (Ord a, Num b) => a -> Map a b -> b
get k m = case Data.Map.lookup k m of
    Nothing -> 0
    Just n -> n



main = do
    s <- getLine
    let m = count s
    let ans = Prelude.map show [get 'A' m, get 'C' m, get 'G' m, get 'T' m]
    putStrLn (Prelude.foldr (\s1 s2 -> s1 ++ " " ++ s2) "" ans)
