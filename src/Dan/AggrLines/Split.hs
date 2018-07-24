{-# LANGUAGE OverloadedStrings #-}
module Dan.AggrLines.Split where

import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Function ((&))
import qualified Data.List as L
import qualified Data.ByteString.Lazy.Char8 as S

type Str = S.ByteString
type Acc = Map.Map Str Int

countGroups :: [Str] -> Acc
countGroups = L.foldl' upd Map.empty . map split
    where
        split :: Str -> (Str, Str)
        split = S.break (==':')
        conv :: Str -> Int
        conv s = S.drop 1 s & S.readInt & fmap fst & fromMaybe 0
        upd :: Acc -> (Str, Str) -> Acc
        upd acc (gr, v) = Map.insertWith (+) gr (conv v) acc


countAndShow = do
    body <- S.readFile "data.txt"
    countGroups (S.lines body)
        & Map.toAscList
        & map (\(k,v)-> S.unpack k <> ":" <> show v)
        & mapM_ putStrLn 
