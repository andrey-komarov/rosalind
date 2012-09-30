import Data.Functor

main = do	
	alpha <- words <$> getLine
	n <- read <$> getLine
	let ans = tail $ f n where
		f 0 = [""]
		f n = "":(alpha >>= \c -> (map (c++) (f (n - 1))))
	putStrLn $ concatMap (++ "\n") ans

