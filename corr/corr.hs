import Control.Applicative
import Control.Monad
import Data.List

complement :: Char -> Char
complement 'A' = 'T'
complement 'T' = 'A'
complement 'G' = 'C'
complement 'C' = 'G'

rc :: String -> String
rc = reverse . map complement

bool2int :: Bool -> Int
bool2int True = 1
bool2int False = 0

hamming :: String -> String -> Int
hamming s1 s2 = sum $ zipWith (\x y -> if x == y then 0 else 1) s1 s2

count :: String -> [String] -> Int
count x = length . elemIndices x

corr :: [String] -> [(String, String)]
corr xs = [(x, correct x) | x <- xs, count x xs' < 2] where
    xs' = xs ++ map rc xs
    correct :: String -> String
    correct s = head $ filter (\x -> count x xs' /= 1) $ filter (\x -> hamming s x == 1) xs'

main = do
    inp <- filter (not . null) <$> lines <$> readFile "corr.txt"
    forM (corr inp) $ \(x, x') ->
        putStr $ x ++ "->" ++ x' ++ "\n"
