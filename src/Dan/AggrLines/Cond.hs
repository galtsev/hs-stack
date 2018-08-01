module Dan.AggrLines.Cond where

import System.IO (stdin)
import Conduit
import Data.Conduit.Zlib (ungzip)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Char8 as S

type Str = S.ByteString
type Acc = Map.Map Str Int

conv :: Str -> Int
conv = fromMaybe 0 . fmap fst . S.readInt . S.drop 1

parseLine :: Str -> (Str, Int)
parseLine = fmap conv . S.break (==':')

upd :: Acc -> (Str, Int) -> Acc
upd acc (gr, v) = Map.insertWith (+) gr v acc

pipe = sourceHandle stdin
    .| ungzip
    .| linesUnboundedAsciiC
    .| mapC parseLine
    .| foldlC upd Map.empty

showAcc :: Acc -> IO ()
showAcc = mapM_ putStrLn . map showLine . Map.toList
    where
        showLine :: (Str, Int) -> [Char]
        showLine (k,v) = S.unpack k <> ":" <> show v

countGroups :: IO ()
countGroups = runConduit pipe >>= showAcc
