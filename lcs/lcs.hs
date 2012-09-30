import Data.List

readLines :: IO [String]
readLines = do
        s <- getLine
        case s of 
                [] -> return []
                _ -> do
                        rest <- readLines
                        return $ s:rest
	
substrings :: Int -> [a] -> [[a]]
substrings n xs = substrings' xs $ drop n xs where 
	substrings' xs [] = [xs]
	substrings' left@(x:xs) (y:ys) = take n left : substrings' xs ys

lcs :: Eq a => [[a]] -> [a]
lcs xss = head $ common len where
	n = length xss
	m = foldr1 min $ map length xss
	len = binsearch 0 (m + 1)
	
	binsearch l r 
		| l + 1 == r = l
		| otherwise = if check c 
			then binsearch c r
			else binsearch l c
		where c = (l + r) `div` 2
	
	common len = foldr1 intersect $ map (substrings len) xss
	
	check = not . null . common

main = do
	xss <- readLines
	putStrLn $ lcs xss
