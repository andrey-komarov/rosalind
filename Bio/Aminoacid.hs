module Bio.Aminoacid where

import Bio.RNA

type Aminoacid = Char

acid :: Codon -> Aminoacid
acid "UUU" = 'F'
acid "UUC" = 'F'
acid "UUA" = 'L'
acid "UUG" = 'L'
acid "UCU" = 'S'
acid "UCC" = 'S'
acid "UCA" = 'S'
acid "UCG" = 'S'
acid "UAU" = 'Y'
acid "UAC" = 'Y'
-- acid "UAA" = undefined
acid "UAA" = ' '
-- acid "UAG" = undefined
acid "UAG" = ' '
acid "UGU" = 'C'
acid "UGC" = 'C'
-- acid "UGA" = undefined
acid "UGA" = ' '
acid "UGG" = 'W'

acid "CUU" = 'L'
acid "CUC" = 'L'
acid "CUA" = 'L'
acid "CUG" = 'L'
acid "CCU" = 'P'
acid "CCC" = 'P'
acid "CCA" = 'P'
acid "CCG" = 'P'
acid "CAU" = 'H'
acid "CAC" = 'H'
acid "CAA" = 'Q'
acid "CAG" = 'Q'
acid "CGU" = 'R'
acid "CGC" = 'R'
acid "CGA" = 'R'
acid "CGG" = 'R'

acid "AUU" = 'I'
acid "AUC" = 'I'
acid "AUA" = 'I'
acid "AUG" = 'M'
acid "ACU" = 'T'
acid "ACC" = 'T'
acid "ACA" = 'T'
acid "ACG" = 'T'
acid "AAU" = 'N'
acid "AAC" = 'N'
acid "AAA" = 'K'
acid "AAG" = 'K'
acid "AGU" = 'S'
acid "AGC" = 'S'
acid "AGA" = 'R'
acid "AGG" = 'R'

acid "GUU" = 'V'
acid "GUC" = 'V'
acid "GUA" = 'V'
acid "GUG" = 'V'
acid "GCU" = 'A'
acid "GCC" = 'A'
acid "GCA" = 'A'
acid "GCG" = 'A'
acid "GAU" = 'D'
acid "GAC" = 'D'
acid "GAA" = 'E'
acid "GAG" = 'E'
acid "GGU" = 'G'
acid "GGC" = 'G'
acid "GGA" = 'G'
acid "GGG" = 'G'

decode :: [RNA] -> [Aminoacid]
decode (x:y:z:xs) = (acid $ x:y:z:[]):decode xs
decode _ = ""
