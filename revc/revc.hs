trans :: Char -> Char
trans 'T' = 'A'
trans 'A' = 'T'
trans 'C' = 'G'
trans 'G' = 'C'

main = do
    s <- getLine
    putStrLn $ map trans $ reverse s
