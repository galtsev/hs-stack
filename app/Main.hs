module Main where

import Options.Applicative
import System.IO (Handle, stdin)

import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Lazy.Char8 as S
import Dan.AggrLines.Types (Str, Acc)

import qualified Dan.AggrLines.Split as Split
import qualified Dan.AggrLines.FromList as FromList
import qualified Dan.AggrLines.Mega as Mega
import qualified Dan.AggrLines.Cond as Cond


showGroups :: Acc -> IO ()
showGroups = mapM_ putStrLn . map showLine . Map.toList
    where
        showLine :: (Str, Int) -> [Char]
        showLine (k,v) = S.unpack k <> ":" <> show v

countAndShow :: ([Str] -> Acc) -> Handle -> IO ()
countAndShow counter h = showGroups . counter =<< fmap S.lines (S.hGetContents h)


main :: IO ()
main = execParser cmdParser >>= cmdToOp
    where
        cmdParser :: ParserInfo [Char]
        cmdParser = info (strArgument mempty) mempty

        cmdToOp :: [Char] -> IO ()
        cmdToOp cmd = case cmd of
            "split" -> countAndShow Split.countGroups stdin
            "from-list" -> countAndShow FromList.countGroups stdin
            "mega" -> countAndShow Mega.countGroups stdin
            "conduit" -> Cond.countGroups

