import Data.Functor

main = do	
	alpha <- words <$> getLine
	n <- getLine
	let ans = iterate (>>= \w -> [w ++ x | x <- alpha]) [""] !! read n
	putStrLn $ concatMap (++ "\n") ans
