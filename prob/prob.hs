import Control.Applicative
import Data.List

log10 :: Double -> Double
log10 x = log x / log 10

prob :: String -> Double -> Double
prob s p = sum $ map (prob' p) s

prob' :: Double -> Char -> Double
prob' p ch
    | ch `elem` "CG" = log10 (p / 2)
    | otherwise = log10 ((1 - p) / 2)

main = do
    inp <- lines <$> readFile "prob.txt"
    let s = inp !! 0
    let gcc = map read $ words $ inp !! 1 :: [Double]
    putStrLn $ intercalate " " $ map (show . prob s) gcc
