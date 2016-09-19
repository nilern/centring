module Main where
import Ctr.Data

import Data.Semigroup
import Options.Applicative

data CliOptions = CliOptions { stx :: Bool }

cliParser :: Parser CliOptions
cliParser = CliOptions
  <$> switch (long "stx" <> help "Just parse and print a syntax object")

main :: IO ()
main = execParser parser >>= putStrLn . show . Bool . stx
  where parser = info (helper <*> cliParser)
                      (fullDesc
                       <> progDesc "Interpret or analyze centring code"
                       <> header "Centring interpreter")
