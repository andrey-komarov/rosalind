import Bio.Aminoacid

decode :: String -> String
decode (x:y:z:xs) = (acid $ x:y:z:[]):decode xs
decode _ = ""

main = do
	s <- getLine
	putStrLn $ decode s