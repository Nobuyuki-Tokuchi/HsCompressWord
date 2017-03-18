import System.Environment (getArgs)
import qualified WordCompress as WComp

data Option =
  Version
  | Help
  | Parameters {
      count :: Int,
      word :: String,
      consonants :: String,
      vowels :: String,
      mode :: String
    }

defaultOption = Parameters {
    count = 4,
    word = "",
    consonants = WComp.defaultConsonants,
    vowels = WComp.defaultVowels,
    mode = "normal"
  }

getOption :: [String] -> Option -> Either String Option
getOption [] Parameters{ word = "" } = Right Help
getOption [] option = Right option
getOption ("-h":xs) _ = Right Help

getOption ["-c"] _ = Left "argument to '-c' is missing"
getOption ("-c":val:xs) option = getOption xs option{ count = (read val) }

getOption ("--normal":xs) option = getOption xs option { mode = "normal" }
getOption ("-n":xs) option = getOption xs option { mode = "normal" }
getOption ("--divide":xs) option = getOption xs option { mode = "divide" }
getOption ("-d":xs) option = getOption xs option { mode = "divide" }

getOption ["--consonants"] _ = Left "argument to '--consonants' is missing"
getOption ("--consonants":val:xs) option = getOption xs option { consonants = val }
getOption ["-C"] _ = Left "argument to '-C' is missing"
getOption ("-C":val@(h:_):xs) option = case h of
  '-' -> Left "argument to '-C' is missing"
  otherwise -> getOption xs option { consonants = val }
getOption ["--vowels"] _ = Left "argument to '--vowels' is missing"
getOption ("--vowels":val:xs) option = getOption xs option { vowels = val }
getOption ["-V"] _ = Left "argument to '-V' is missing"
getOption ("-V":val:xs) option = getOption xs option { vowels = val }

getOption (inWord:xs) option = getOption xs option{ word = inWord }

help :: IO()
help = do
  putStrLn $ concatMap (\x -> x ++ "\n") [
      "hah (-n|-d) (-C) (-V) (-c n) word",
      "  -c n : Split width. (default) -c 4",
      "  -n, --normal : Hah Compress. (default)",
      "  -d, --divide : DivideHah Compress.",
      "  -C, --consonants : Set consonants. (DivideHah only) (default) -C bcdfghjklmnprqstvwxyz",
      "  -V, --vowels : Set consonants. (DivideHah only) (default) -V aeiou"
    ]

run :: Either String Option -> IO()
run (Left message) = putStrLn message
run (Right Help) = help
run (Right Parameters { count = c, word = w, mode = m, consonants = cs, vowels = vs }) = putStrLn $ WComp.compress compMode w where
  compMode = case m of
    "divide" -> (WComp.DivideHah c cs vs)
    otherwise -> (WComp.Hah c)

main = do
  args <- getArgs
  run $ getOption args defaultOption

