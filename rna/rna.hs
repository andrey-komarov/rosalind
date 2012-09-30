replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ [] = []
replace x b (y:xs) = (if (x == y) then b else y):(replace x b xs)

main = do
    s <- getLine
    putStrLn (replace 'T' 'U' s)
