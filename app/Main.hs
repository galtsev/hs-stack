module Main where

import Options.Applicative

import qualified Dan.AggrLines.Split as Split
import qualified Dan.AggrLines.FromList as FromList
import qualified Dan.AggrLines.Mega as Mega

main :: IO ()
main = do
        cmd <- execParser cmdParser
        case cmd of
            "split" -> Split.countAndShow
            "from-list" -> FromList.countAndShow
            "mega" -> Mega.countAndShow "data.txt"
    where
        cmdParser = info (strArgument mempty) mempty
        --cmdOpt = strOption (long "cmd")
        --cmdArg = argument str