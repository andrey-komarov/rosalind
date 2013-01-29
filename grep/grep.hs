import Prelude hiding (all)
import Control.Applicative
import Data.List (tails, inits, isPrefixOf)
import Data.Array
import qualified Data.Set as S
import qualified Data.Map as M
import Debug.Trace
import Data.Monoid
import Control.Monad

complement :: Char -> Char
complement 'A' = 'T'
complement 'T' = 'A'
complement 'G' = 'C'
complement 'C' = 'G'

rc :: String -> String
rc = reverse . map complement

overlap :: Int -> String -> String -> Int
overlap bound s1 s2 = head $ [i | i <- [min n m, (min n m) - 1..bound], 
                                take i s2 == reverse (take i s1')] ++ [0] where
    n = length s1
    m = length s2
    s1' = reverse s1

merge :: String -> String -> String
merge s1 s2 = s1 ++ drop (overlap 0 s1 s2) s2

substrings :: Int -> String -> [String]
substrings n a = map (take n) $ take (length a - n + 1) $ tails a

type Graph = M.Map String [String]

dbru :: Int -> [String] -> Graph
dbru k xs = M.fromListWith (<>) $ [(init s, [tail s]) | ss <- xs, s <- substrings (k + 1) ss]

edge :: Graph -> String -> Maybe String
edge g v = case M.lookup v g of
    Nothing -> Nothing
    Just s -> return $ head s

vertex :: Graph -> String
vertex g = fst $ M.findMin g

unjust :: Maybe a -> a
unjust (Just a) = a

debug :: Show a => a -> a
debug a = trace (show a) a

paths :: Int -> String -> Graph -> [[String]]
paths remain v g 
-- | trace (show v ++ " " ++ show g) False = undefined
 | remain == 0 = [[]]
 | otherwise = let es = g M.! v in map (v:) [ p
                        | (a, e:b) <- zip (inits es) (init $ tails es)
                        , not (e `elem` a)
                        , p <- paths (remain - 1) e (M.insert v (a++b) g)]

cov :: [String] -> [String]
cov xs = map (map head) p where
    start = init $ head xs
    k = length $ head xs 
    g = dbru (k-1) xs
    edges = length $ concat $ M.elems g
    p = paths edges start g

main = do
    inp <- filter (not . null) <$> lines <$> readFile "grep.txt"
    putStrLn $ concatMap (++"\n") $ filter (head inp `isPrefixOf`) $ cov inp
