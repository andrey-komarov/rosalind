hamm :: (Eq a, Num b) => [a] -> [a] -> b
hamm [] [] = 0
hamm (x:xs) (y:ys) 
    | x == y    = rest
    | otherwise = 1 + rest
    where rest = hamm xs ys

main = do
    s1 <- getLine
    s2 <- getLine
    print $ hamm s1 s2
