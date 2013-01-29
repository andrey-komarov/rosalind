import Control.Applicative
import Data.List
import Data.Array
import qualified Data.Set as S
import Debug.Trace

overlap :: String -> String -> Int
overlap s1 s2 = head [i | i <- [min n m, (min n m) - 1..0], 
                                take i s2 == reverse (take i s1')] where
    n = length s1
    m = length s2
    s1' = reverse s1

merge :: String -> String -> String
merge s1 s2 = s1 ++ drop (overlap s1 s2) s2

type Graph = Array Int [Int]

dfs :: Int -> Graph -> S.Set Int -> Int -> [[Int]]
dfs n g used v 
 | S.size used == n = [[v]]
 | otherwise =  [v:path | v' <- g ! v, 
                          not (v' `S.member` used),
                          path <- dfs n g (S.insert v' used) v' ]

cov :: [String] -> String
cov xs = foldl1 merge $ map (strs !) $ head p where
    strs = listArray (0, n-1) xs
    xs' = zip [0..] xs
    n = length xs
    m = length $ head xs

    p = concat [dfs n g (S.singleton v) v | v <- [0..n-1]]

    g :: Graph
    g = array (0, n-1) [
        (i, [j | (j, s2) <- xs', overlap s1 s2 >= m `div` 2])
        | (i, s1) <- xs' ]

main = do
    inp <- filter (not . null) <$> lines <$> readFile "long.txt"
    putStrLn $ cov inp
