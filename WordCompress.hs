module WordCompress(
  WordCompress(..), compress,
  defaultOrder, defaultConsonant, defaultVowel
) where

data WordCompress =
  Hah { width :: Int } |
  DivideHah { width :: Int, consonants :: String, vowels :: String }

hah_compress :: [a] -> Int -> [a]
hah_compress [] _  = []
hah_compress [ch] _ = [ch]
hah_compress str width = [head x, last x] ++ hah_compress (drop width str) width
  where x = take width str

compress :: WordCompress -> String -> String
compress (Hah width) str = hah_compress str width

defaultOrder = "abcdefghijklmnopqrstuvwxyz"
defaultConsonant = "bcdfghjklmnpqrstvwxyz"
defaultVowel = "aeiou"
