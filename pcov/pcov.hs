import Control.Applicative
import Data.List
import Data.Array
import qualified Data.Set as S
import Debug.Trace

overlap :: Int -> String -> String -> Int
overlap bound s1 s2 = head $ [i | i <- [min n m, (min n m) - 1..bound], 
                                take i s2 == reverse (take i s1')] ++ [0] where
    n = length s1
    m = length s2
    s1' = reverse s1

merge :: String -> String -> String
merge s1 s2 = s1 ++ drop (overlap 0 s1 s2) s2

type Graph = Array Int [Int]

dfs :: Int -> Graph -> S.Set Int -> Int -> [[Int]]
dfs n g used v 
 | S.size used == n = [[v]]
 | otherwise =  [v:path | v' <- g ! v, 
                          not (v' `S.member` used),
                          path <- dfs n g (S.insert v' used) v' ]

cov :: [String] -> String
cov xs = drop (m - 1) $ foldl1 merge $ map (strs !) $ head p where
    strs = listArray (0, n-1) xs
    xs' = zip [0..] xs
    n = length xs
    m = length $ head xs

    p = concat [dfs n g (S.singleton v) v | v <- [0..n-1]]

    g :: Graph
    g = array (0, n-1) [
        (i, [j | (j, s2) <- xs', overlap (m - 1) s1 s2 == m - 1])
        | (i, s1) <- xs' ]

main = do
    inp <- filter (not . null) <$> lines <$> readFile "pcov.txt"
    putStrLn $ cov inp
