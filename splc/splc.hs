import Data.List(isPrefixOf)
import Bio.Aminoacid(decode)

readLines :: IO [String]
readLines = do
        s <- getLine
        case s of 
                [] -> return []
                _ -> do
                        rest <- readLines
                        return $ s:rest

deleteOccurences :: Eq a => [a] -> [a] -> [a]
deleteOccurences p t = delete t where
    n = length p
    delete [] = []
    delete t@(x:xs) 
        | p `isPrefixOf` t = delete $ drop n t
        | otherwise = x:delete xs 

replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ [] = []
replace a b (x:xs) 
    | a == x = b:rest
    | otherwise = x:rest
    where rest = replace a b xs

main = do
    s <- getLine
    ss <- readLines
    putStrLn $ decode $ replace 'T' 'U' $ foldr deleteOccurences s ss 
