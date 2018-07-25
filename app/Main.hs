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
        countAndShow (cmdToOp cmd) stdin
    where
        cmdParser :: ParserInfo [Char]
        cmdParser = info (strArgument mempty) mempty

        cmdToOp :: [Char] -> ([Str] -> Acc)
        cmdToOp cmd = case cmd of
            "split" -> Split.countGroups
            "from-list" -> FromList.countGroups
            "mega" -> Mega.countGroups

