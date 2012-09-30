sseq :: Eq a => [a] -> [a] -> Maybe [Int]
sseq p t = sseq' p t 1 where
    sseq' [] _ _ = Just []
    sseq' _ [] _ = Nothing
    sseq' p@(x:xs) (y:ys) i
        | x == y = sseq' xs ys (i+1) >>= \s -> Just $ i:s 
        | otherwise = sseq' p ys (i+1)

main = do
    s <- getLine
    p <- getLine
    putStrLn $ case sseq p s of
        Just xs -> concatMap ((++" ").show) xs
