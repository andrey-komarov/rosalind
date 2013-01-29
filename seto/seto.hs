import qualified Data.Set as S
import Control.Applicative
import Data.List
import Control.Monad

readSet :: String -> S.Set Int
readSet s = S.fromList $ map read $ words $ filter (not . (`elem` "{,}")) s

pp :: S.Set Int -> String
pp s = "{" ++ intercalate ", " (map show $ S.toList s) ++ "}"

main = do
    inp <- lines <$> readFile "seto.txt"
    let n = read (inp !! 0) :: Int
    let u = S.fromList [1..n]
    let a = readSet (inp !! 1)
    let b = readSet (inp !! 2)
    forM [a `S.union` b, a `S.intersection` b, a S.\\ b, b S.\\ a, u S.\\ a, u S.\\ b] $ \i ->
        putStrLn $ pp i
