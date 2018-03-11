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
  SeparateHah { width :: Int, consonants :: String, vowels :: String } |
  Matrix |
  DivMatrix { consonants :: String, vowels :: String }

hahCompress :: Int -> [a] -> [a]
hahCompress _ []  = []
hahCompress _ [ch] = [ch]
hahCompress w str = let x = take w str
                    in head x : last x : hahCompress w (drop w str)

matrixCompress :: [a] -> [a]
matrixCompress [] = []
matrixCompress str = head str : nextCompress (ceiling (sqrt $ fromIntegral $ length str :: Double)) (tail str)
  where nextCompress _ [] = []
        nextCompress size subStr
                      | size >= length subStr = [last subStr]
                      | otherwise = let nextStr = drop size subStr
                                    in head nextStr : nextCompress size (tail nextStr)

divcv :: String -> String -> [(Char, Int)] -> ([(Char, Int)], [(Char, Int)])
divcv cs vs tlist =
  let clist = filter (divider cs) tlist
      vlist = filter (divider vs) tlist
  in (clist, vlist) where divider param (str, _) = str `elem` param

sepcv :: String -> String -> String -> [String]
sepcv _ _ [] = []
sepcv cs vs tlist =
  let sepc = span (\x -> any (== x) cs) tlist
      sepv = span (\x -> any (== x) vs) $ snd sepc
  in (fst sepc) : (fst sepv) : (sepcv cs vs $ snd sepv)

sortFst :: [(Char, Int)] -> String
sortFst tlist = map fst $ List.sortBy (Ord.comparing snd) tlist

divCompress :: (([(Char, Int)], [(Char, Int)]) -> [(Char, Int)]) -> String -> String -> String -> String
divCompress comp cs vs str = sortFst $ comp $ divcv cs vs $ zip str [0..]

divhahCompress :: WordCompress -> String -> String
divhahCompress (DivideHah w cs vs) str = divCompress comp cs vs str
  where comp (clist, vlist) = hahCompress w clist ++ hahCompress w vlist
divhahCompress _ _ = ""

divmatrixCompress :: WordCompress -> String -> String
divmatrixCompress (DivMatrix cs vs) str = divCompress comp cs vs str
  where comp (clist, vlist) = matrixCompress clist ++ matrixCompress vlist
divmatrixCompress _ _ = ""

sephahCompress :: WordCompress -> String -> String
sephahCompress (SeparateHah w cs vs) str = concat $ hahCompress w $ sepcv cs vs str
sephahCompress _ _ = ""

compress :: WordCompress -> String -> String
compress (Hah w) str = hahCompress w str
compress Matrix str = matrixCompress str
compress divhah@DivideHah{} str = divhahCompress divhah str
compress divmatrix@DivMatrix{} str = divmatrixCompress divmatrix str
compress sephah@SeparateHah{} str = sephahCompress sephah str 

--defaultOrder = "abcdefghijklmnopqrstuvwxyz"
defaultConsonants :: String
defaultConsonants = "bcdfghjklmnpqrstvwxyz"

defaultVowels :: String
defaultVowels = "aeiou"
