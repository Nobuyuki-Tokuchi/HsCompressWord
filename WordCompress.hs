module WordCompress(
  WordCompress(..), compress,
  --defaultOrder,
  defaultConsonants, defaultVowels
) where

import qualified Data.List as List;
import qualified Data.Ord as Ord;

data WordCompress =
  Hah { width :: Int } |
  DivideHah { width :: Int, consonants :: String, vowels :: String } |
  Matrix |
  DivMatrix { consonants :: String, vowels :: String }

hah_compress :: Int -> [a] -> [a]
hah_compress _ []  = []
hah_compress _ [ch] = [ch]
hah_compress width str = let x = take width str
                         in [head x, last x] ++ hah_compress width (drop width str)

matrix_compress :: [a] -> [a]
matrix_compress [] = []
matrix_compress str = [head str] ++ next_compress (fromIntegral $ ceiling $ sqrt $ fromIntegral $ length str) (tail str)
  where next_compress _ [] = []
        next_compress size str
                      | size >= length str = [last str]
                      | otherwise = let nextStr = drop size str
                                    in [head nextStr] ++ next_compress size (tail nextStr)

divcv :: String -> String -> [(Char, Int)] -> ([(Char, Int)], [(Char, Int)])
divcv consonants vowels tlist =
  let clist = filter (divider consonants) tlist
      vlist = filter (divider vowels) tlist
  in (clist, vlist) where divider param = \(str, _) -> elem str param

sort_fst :: [(Char, Int)] -> String
sort_fst tlist = map (\(ch, _) -> ch) $ List.sortBy (Ord.comparing snd) tlist

divhah_compress :: WordCompress -> String -> String
divhah_compress (DivideHah width consonants vowels) str =
  sort_fst $ compress $ divcv consonants vowels $ zip str [0..]
  where compress (clist, vlist) = hah_compress width clist ++ hah_compress width vlist

divmatrix_compress :: WordCompress -> String -> String
divmatrix_compress (DivMatrix consonants vowels) str = 
  sort_fst $ compress $ divcv consonants vowels $ zip str [0..]
  where compress (clist, vlist) = matrix_compress clist ++ matrix_compress vlist

compress :: WordCompress -> String -> String
compress (Hah width) str = hah_compress width str
compress (Matrix) str = matrix_compress str
compress divhah@(DivideHah _ _ _) str = divhah_compress divhah str
compress divmatrix@(DivMatrix _ _) str = divmatrix_compress divmatrix str

--defaultOrder = "abcdefghijklmnopqrstuvwxyz"
defaultConsonants = "bcdfghjklmnpqrstvwxyz"
defaultVowels = "aeiou"

