module Dan.AggrLines.Mega where

import Data.Word (Word8)
import qualified Data.Map.Strict as M
import Data.Void (Void)
import qualified Data.ByteString.Lazy.Char8 as S
import Text.Megaparsec
import Text.Megaparsec.Byte (char)
import Data.Foldable
import Data.Maybe (fromMaybe)
import Text.Megaparsec.Byte.Lexer (decimal)

type Str = S.ByteString
type Acc = M.Map Str Int

type Parser = Parsec Void Str

sep :: Word8
sep = toEnum $ fromEnum ':'

lp2 :: Parser (Str, Int)
lp2 = (,) <$> takeWhileP Nothing (/=sep) <* char sep <*> decimal

errGroupName :: Str
errGroupName = S.pack "errors"

cntGrps :: Str -> Acc
cntGrps = M.fromListWith (+) . fmap parseLine . S.lines
    where
        parseLine :: Str -> (Str, Int)
        parseLine = fromMaybe (errGroupName, 1) . parseMaybe lp2

countAndShow fn = mapM_ putStrLn . map showLine . M.toList . cntGrps =<< S.readFile fn
    where
        showLine :: (Str, Int) -> [Char]
        showLine (k,v) = S.unpack k <> ":" <> show v