permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = permutations' [] x xs where
	permutations' left m [] = map (m:) $ permutations left
	permutations' left m right@(r:rx) =
		(map (m:) $ permutations $ left ++ right) ++
		(permutations' (left ++ [m]) r rx)

matrixPrint :: Show a => [[a]] -> String
matrixPrint a = concatMap makeLine aStr where
	makeLine = (++ "\n") . concatMap (++ " ")
	aStr = map (map show) a

main = do
	s <- getLine
	let n = (read :: String -> Int) s
	let perms = permutations [1..n]
	print $ length perms
	putStrLn $ matrixPrint perms
