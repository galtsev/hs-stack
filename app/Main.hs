module Main where

import Options.Applicative
import System.IO (Handle, stdin)

import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Lazy.Char8 as S
import Dan.AggrLines.Types (Str, Acc)

import qualified Dan.AggrLines.Split as Split
import qualified Dan.AggrLines.FromList as FromList
import qualified Dan.AggrLines.Mega as Mega

countAndShow :: ([Str] -> Acc) -> Handle -> IO ()
countAndShow counter h = mapM_ putStrLn . map showLine . Map.toList . counter =<< fmap S.lines (S.hGetContents h)
    where
        showLine :: (Str, Int) -> [Char]
        showLine (k,v) = S.unpack k <> ":" <> show v

main :: IO ()
main = do
        cmd <- execParser cmdParser
        case cmd of
            "split" -> countAndShow Split.countGroups stdin
            "from-list" -> countAndShow FromList.countGroups stdin
            "mega" -> Mega.countAndShow "data.txt"
    where
        cmdParser = info (strArgument mempty) mempty
        --cmdOpt = strOption (long "cmd")
        --cmdArg = argument str