import Control.Applicative

main = do
    [k, m, n] <- map read <$> words <$> readFile "iprb.txt"
    print $ 1 - (4 * n * (n - 1) + m * (m - 1) + 4 * n * m) / (n + m + k) / (n + m + k - 1) / 4
