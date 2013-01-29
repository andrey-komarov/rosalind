import Control.Applicative
import Data.List

kmers n = iterate (>>= \w -> [w ++ x | x <- ["A", "C", "G", "T"]]) [""] !! n

count :: Eq a => [a] -> [a] -> Int
count x s = length $ [() | y <- tails s, x `isPrefixOf` y]

kmercomp :: Int -> String -> [Int]
kmercomp k s = map (`count` s) $ kmers k where

main = do
    inp <- concat <$> filter (\s -> not (null s) && head s /= '>') <$> lines <$> readFile "kmer.txt"
    putStrLn $ intercalate " " $ map show $ kmercomp 4 inp
