import Data.Array

lcs :: Eq a => [a] -> [a] -> [a]
lcs xs ys = reverse answer where
	n = length xs
	m = length ys
	a = array (1, n) $ zip [1..] xs
	b = array (1, m) $ zip [1..] ys
	dp = res where 
		res = array ((0, 0), (n, m)) $ 
			[((0, i), 0) | i <- [0..m]] ++ 
			[((i, 0), 0) | i <- [1..n]] ++
			[((i, j), if (a ! i == b ! j) 	
			 then (res ! (i - 1, j - 1) + 1)
			 else max (res ! (i - 1, j)) (res ! (i, j - 1))) 
			| i <- [1..n], j <- [1..m]
			]
	answer = construct n m where
		construct 0 _ = []
		construct _ 0 = []
		construct n m = if a ! n == b ! m
			then a ! n : construct (n - 1) (m - 1)
			else if dp ! (n - 1, m) > dp ! (n, m - 1)
				then construct (n - 1) m
				else construct n (m - 1) 
	
readLines :: IO [String]
readLines = do
        s <- getLine
        case s of 
                [] -> return []
                _ -> do
                        rest <- readLines
                        return $ s:rest
	
main = do
    s1 <- getLine
    s2 <- getLine
    putStrLn $ lcs s1 s2 

