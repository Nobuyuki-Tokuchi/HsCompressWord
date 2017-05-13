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
-- value of splitting word
getOption [op@"-c"] _ = Left ("argument to '" ++ op ++ "' is missing")
getOption (op@"-c":val@(h:_):xs) option = case h of
  '-' -> Left ("argument to '" ++ op ++ "' is missing")
  otherwise -> getOption xs option{ count = (read val) }
-- mode option
getOption ("--normal":xs) option = getOption xs option { mode = "normal" }
getOption ("-n":xs) option = getOption xs option { mode = "normal" }
getOption ("--divide":xs) option = getOption xs option { mode = "divide" }
getOption ("-d":xs) option = getOption xs option { mode = "divide" }
getOption ("--matix":xs) option = getOption xs option { mode = "matrix" }
getOption ("-m":xs) option = getOption xs option { mode = "matrix" }
getOption ("--divmat":xs) option = getOption xs option { mode = "divmat" }
getOption ("-dm":xs) option = getOption xs option { mode = "divmat" }
-- consonants option
getOption [op@"--consonants"] _ = Left ("argument to '" ++ op ++ "' is missing")
getOption (op@"--consonants":val@(h:_):xs) option = case h of
  '-' -> Left ("argument to '" ++ op ++ "' is missing")
  otherwise -> getOption xs option { consonants = val }
getOption [op@"-C"] _ = Left ("argument to '" ++ op ++ "' is missing")
getOption (op@"-C":val@(h:_):xs) option = case h of
  '-' -> Left ("argument to '"++ op ++"' is missing")
  otherwise -> getOption xs option { consonants = val }
-- vowels option
getOption ["--vowels"] _ = Left "argument to '--vowels' is missing"
getOption (op@"--vowels":val@(h:_):xs) option = case h of
  '-' -> Left ("argument to '"++ op ++"' is missing")
  otherwise -> getOption xs option { vowels = val }
getOption ["-V"] _ = Left "argument to '-V' is missing"
getOption (op@"-V":val@(h:_):xs) option = case h of
  '-' -> Left ("argument to '"++ op ++"' is missing")
  otherwise -> getOption xs option { vowels = val }
-- other
getOption (inWord:xs) option = getOption xs option{ word = inWord }

help :: IO()
help = do
  putStrLn $ concatMap (\x -> x ++ "\n") [
      "hah (-n|-d) (-C) (-V) (-c n) word",
      "  -c n : Split width. (default) -c 4",
      "  -n, --normal : Hah Compress. (default)",
      "  -d, --divide : DivideHah Compress.",
      "  -m, --matrix : Matix Compress.",
      "  -dm, --divmat : DivideMatix Compress.",
      "  -C, --consonants : Set consonants. (DivideHah only) (default) -C bcdfghjklmnprqstvwxyz",
      "  -V, --vowels : Set consonants. (DivideHah only) (default) -V aeiou"
    ]

run :: Either String Option -> IO()
run (Left message) = putStrLn message
run (Right Help) = help
run (Right Parameters { count = c, word = w, mode = m, consonants = cs, vowels = vs }) = putStrLn $ WComp.compress compMode w where
  compMode = case m of
    "divide" -> (WComp.DivideHah c cs vs)
    "divmat" -> (WComp.DivMatrix cs vs)
    "matrix" -> (WComp.Matrix)
    otherwise -> (WComp.Hah c)

main = do
  args <- getArgs
  run $ getOption args defaultOption

