import Prelude hiding (all)
import Control.Applicative
import Data.List (tails)
import Data.Array
import qualified Data.Set as S
import qualified Data.Map as M
import Debug.Trace
import Data.Monoid
import Data.Foldable

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

type Graph = M.Map String (S.Set String)

dbru :: Int -> [String] -> Graph
dbru k xs = M.fromListWith (<>) $ [(init s, S.singleton $ tail s) | ss <- xs, s <- substrings (k + 1) ss]

edge :: Graph -> String -> Maybe String
edge g v = case M.lookup v g of
    Nothing -> Nothing
    Just s -> return $ S.findMin s

vertex :: Graph -> String
vertex g = fst $ M.findMin g

removeEdge :: (String, String) -> Graph -> Graph
removeEdge (s1, s2) g = g'' where
    g' = M.update (return . S.delete s2) s1 g
    g'' = if S.null (g' M.! s1) then M.delete s1 g'
                                else g'

unjust :: Maybe a -> a
unjust (Just a) = a

removeCycle :: Graph -> Graph
removeCycle g = let
        start = vertex g
        first = unjust $ edge g start

        rem :: String -> Graph -> String -> Maybe Graph
        rem need g v
         | v == need = Just g
         | otherwise = case edge g v of
            Nothing -> Nothing
            Just v' -> rem need (removeEdge (v, v') g) v'
    in case rem start (removeEdge (start, first) g) first of
        Nothing -> g 
        Just g' -> g' 

isTwoCycles :: Graph -> Bool
isTwoCycles g = all ((==1) . S.size) g && (M.null $ (removeCycle . removeCycle) g) 

toPath :: Graph -> [String]
toPath g = toPath' (vertex g) g where
    toPath' v g = case edge g v of
        Nothing -> []
        Just v' -> v : toPath' v' (removeEdge (v, v') g)

cov :: [String] -> String
cov xs = map head path where
    path = toPath $ removeCycle $ dbru k $ xs ++ map rc xs
    m = length $ head xs
    k = head $ [i | i <- [m-1,m-2.. 0], isTwoCycles (dbru i $ xs ++ map rc xs)]

main = do
    inp <- filter (not . null) <$> lines <$> readFile "gasm.txt"
    putStrLn $ cov inp
