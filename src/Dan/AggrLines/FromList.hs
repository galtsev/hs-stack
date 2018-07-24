{-# LANGUAGE OverloadedStrings #-}
module Dan.AggrLines.FromList where

import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Function ((&))
import qualified Data.List as L
import qualified Data.ByteString.Lazy.Char8 as S

type Str = S.ByteString
type Acc = Map.Map Str Int

cntGrps :: Str -> Acc
cntGrps = Map.fromListWith (+) . fmap parseLine . S.lines
    where
        conv :: Str -> Int
        conv = fromMaybe 0 . fmap fst . S.readInt . S.drop 1
        parseLine :: Str -> (Str, Int)
        parseLine = fmap conv . S.break (==':')

countAndShow = mapM_ putStrLn . map showLine . Map.toList . cntGrps =<< S.readFile "data.txt"
    where
        showLine :: (Str, Int) -> [Char]
        showLine (k,v) = S.unpack k <> ":" <> show v
