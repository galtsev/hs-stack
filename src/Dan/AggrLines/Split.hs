{-# LANGUAGE OverloadedStrings #-}
module Dan.AggrLines.Split (countGroups) where

import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Function ((&))
import Data.List (foldl')
import qualified Data.ByteString.Lazy.Char8 as S
import Dan.AggrLines.Types (Str, Acc)


countGroups :: [Str] -> Acc
countGroups = foldl' upd Map.empty . map split
    where
        split :: Str -> (Str, Str)
        split = S.break (==':')
        conv :: Str -> Int
        conv s = S.drop 1 s & S.readInt & fmap fst & fromMaybe 0
        upd :: Acc -> (Str, Str) -> Acc
        upd acc (gr, v) = Map.insertWith (+) gr (conv v) acc
