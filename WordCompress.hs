module WordCompress(
  WordCompress(..), compress,
  defaultOrder, defaultConsonants, defaultVowels
) where

import qualified Data.List as List;
import qualified Data.Ord as Ord;

data WordCompress =
  Hah { width :: Int } |
  DivideHah { width :: Int, consonants :: String, vowels :: String }

hah_compress :: [a] -> Int -> [a]
hah_compress [] _  = []
hah_compress [ch] _ = [ch]
hah_compress str width = let x = take width str
                         in [head x, last x] ++ hah_compress (drop width str) width

divhah_compress :: WordCompress -> String -> String
divhah_compress (DivideHah width consonants vowels) str = map (\x@(ch, _) -> ch) $ List.sortBy (Ord.comparing snd) $ divcv $ zip str [0..]
  where divcv tlist = let clist = filter (\x@(str, _) -> elem str consonants) tlist
                          vlist = filter (\x@(str, _) -> elem str vowels) tlist
                      in hah_compress clist width ++ hah_compress vlist width

compress :: WordCompress -> String -> String
compress (Hah width) str = hah_compress str width
compress divhah str = divhah_compress divhah str

defaultOrder = "abcdefghijklmnopqrstuvwxyz"
defaultConsonants = "bcdfghjklmnpqrstvwxyz"
defaultVowels = "aeiou"

