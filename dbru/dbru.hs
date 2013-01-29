import Control.Applicative
import qualified Data.Set as S
import Control.Monad

type Edge = (String, String)

complement :: Char -> Char
complement 'A' = 'T'
complement 'T' = 'A'
complement 'G' = 'C'
complement 'C' = 'G'

rc :: String -> String
rc = reverse . map complement

dbru :: [String] -> [Edge]
dbru xs = S.toList $ S.fromList $ map (\s -> (init s, tail s)) $ xs ++ map rc xs

main = do
    inp <- filter (not . null) <$> lines <$> readFile "dbru.txt"
    forM (dbru inp) $ \(from, to) ->
        putStrLn $ "(" ++ from ++ ", " ++ to ++ ")"
