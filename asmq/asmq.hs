import qualified Data.ByteString.Char8 as BS
import Data.List
import Control.Applicative
import System.IO

toContigLengths :: [BS.ByteString] -> [Int]
toContigLengths = sort . map BS.length

mean :: [Int] -> Double
mean a = (fromIntegral $ sum a) / (fromIntegral $ length a)

prefixSums :: Int -> [Int] -> [Int]
prefixSums start [] = []
prefixSums start (x:xs) = let x' = x + start
                          in x':prefixSums x' xs

metric :: Int -> Int -> [Int] -> Int
metric n1 n2 a = snd $ head $ aa where
    sumLen = sum a
    aa = dropWhile (\(x, y) -> x * n1 < (sumLen - x) * n2) $ zip (prefixSums 0 a) a

main = do
    inp <- BS.lines <$> BS.readFile "asmq.txt"
    let contigs = toContigLengths inp
    print $ metric 1 1 contigs
    print $ metric 3 1 contigs
