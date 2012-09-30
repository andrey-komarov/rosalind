startsWith :: Eq a => [a] -> [a] -> Bool
startsWith _ [] = True
startsWith [] _ = False
startsWith (x:xs) (y:ys)
    | x == y    = startsWith xs ys
    | otherwise = False

findSubstr :: Eq a => [a] -> [a] -> [Int]
findSubstr s t = findSubstr' s t 0 where
    findSubstr' _ [] _ = []
    findSubstr' xs s@(y:ys) n
        | s `startsWith` xs = n:rest
        | otherwise       = rest
        where rest = findSubstr' xs ys (n + 1)

main = do
    s <- getLine
    t <- getLine
    let res = map (\x -> x + 1) $ findSubstr t s
    putStrLn $ foldr (\s1 s2 -> s1 ++ " " ++ s2) "" $ map show res
