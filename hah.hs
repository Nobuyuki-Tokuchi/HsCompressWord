import System.Environment (getArgs)
import WordCompress as WComp

data Option =
  Version
  | Help
  | Parameters {
      count :: Int,
      word :: String
    }

defaultOption = Parameters {
    count = 4,
    word = ""
  }

getOption :: [String] -> Option -> Either String Option
getOption [] Parameters{ word = "" } = Right Help
getOption [] option = Right option
getOption ("-h":xs) _ = Right Help
getOption ["-c"] _ = Left "argument to '-c' is missing "
getOption ("-c":val:xs) option = getOption xs option{ count = (read val) }
getOption (inWord:xs) option = getOption xs option{ word = inWord }

help :: IO()
help = do
  putStrLn "hah (-c n) word"
  putStrLn "-c n : Split width. (default) -c 4"

run :: Either String Option -> IO()
run (Left message) = putStrLn message
run (Right Help) = help
run (Right Parameters{ count = c, word = w }) = putStrLn $ WComp.compress (WComp.Hah c) w

main = do
  args <- getArgs
  run $ getOption args defaultOption


