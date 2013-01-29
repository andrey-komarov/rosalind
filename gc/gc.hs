import Control.Applicative
import Data.List

gc :: [String] -> (Double, String)
gc xs = (gcCnt, name) where
    name = tail $ head xs
    seq = concat $ tail xs
    gcCnt = (fromIntegral $ length $ filter (`elem` "GC") seq) /
            (fromIntegral $ length seq)

gcContent :: [String] -> [(Double, String)]
gcContent = map gc . groupBy (\x y -> head y /= '>')

main = do
    inp <- lines <$> readFile "gc.txt"
    putStrLn $ let (gc, name) = maximum $ gcContent inp
            in name ++ "\n" ++ show (100 * gc) ++ "%"
