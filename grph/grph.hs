import Control.Applicative
import Data.List

startsWith :: String -> String -> Bool
_ `startsWith` [] = True
[] `startsWith` _ = False
(x:xs) `startsWith` (y:ys)
    | x == y = xs `startsWith` ys
    | otherwise = False

type Node = (String, String)

overlaps :: Node -> Node -> Bool
overlaps (_, s1) (_, s2) = s2 `startsWith` drop (length s1 - 3) s1

readLines :: IO [String]
readLines = do
        s <- getLine
        case s of 
                [] -> return []
                _ -> do
                        rest <- readLines
                        return $ s:rest

nodes :: [String] -> [Node]
nodes [] = []
nodes (x:xs) = (x, concat first):nodes rest where
        (first, rest) = break (`startsWith` ">") xs

getOverlaps :: [Node] -> [(Node, Node)]
getOverlaps xs = [(x, y) | x <- xs, y <- xs, x /= y, overlaps x y]

main = do
    nodes <- nodes <$> readLines
    let ans = sort $ getOverlaps nodes
    let peekNames = map (\((name1, _), (name2, _)) -> tail name1 ++ " " ++ tail name2)
    putStrLn $ concatMap (++ "\n") $ peekNames ans
