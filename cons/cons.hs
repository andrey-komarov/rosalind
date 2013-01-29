import Control.Applicative
import Data.List

makeOne :: String -> (Char, Int, Int, Int, Int)
makeOne s = (letter, a, c, g, t) where
    [a, c, g, t] = map (length . (`elemIndices` s)) "ACGT"
    letter = snd $ maximum [(a, 'A'), (c, 'C'), (g, 'G'), (t, 'T')]

make :: [String] -> [(Char, Int, Int, Int, Int)]
make xs = case length $ head xs of
    0 -> []
    n -> makeOne (map head xs) : make (map tail xs)

main = do
    inp <- filter (not . null) <$> lines <$> readFile "cons.txt"
    let a = make inp
    putStrLn $ [c | (c,_,_,_,_) <- a]
    putStrLn $ "A:" ++ concat [" " ++ show i | (_,i,_,_,_) <- a]
    putStrLn $ "C:" ++ concat [" " ++ show i | (_,_,i,_,_) <- a]
    putStrLn $ "G:" ++ concat [" " ++ show i | (_,_,_,i,_) <- a]
    putStrLn $ "T:" ++ concat [" " ++ show i | (_,_,_,_,i) <- a]
