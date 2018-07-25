{-# LANGUAGE OverloadedStrings #-}
module Dan.AggrLines.FromList (countGroups) where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy.Char8 as S
import Dan.AggrLines.Types (Str, Acc)


countGroups :: [Str] -> Acc
countGroups = Map.fromListWith (+) . fmap parseLine
    where
        conv :: Str -> Int
        conv = fromMaybe 0 . fmap fst . S.readInt . S.drop 1
        parseLine :: Str -> (Str, Int)
        parseLine = fmap conv . S.break (==':')
