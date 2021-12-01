module App where

import           CommandLine         (cmdParser)
import           Options.Applicative (execParser)

main :: IO ()
main = do
    cmdConfig <- execParser cmdParser
    print cmdConfig
