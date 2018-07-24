{-# LANGUAGE OverloadedStrings #-}
module Dan.AggrLines.FromList where

import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Function ((&))
import qualified Data.List as L
import qualified Data.ByteString.Lazy.Char8 as S

type Str = S.ByteString
type Acc = Map.Map Str Int

parseLine :: Str -> (Str, Int)
parseLine s = (gr, fromMaybe 0 . fmap fst . S.readInt . S.drop 1 $ vv)
    where
        (gr, vv) = S.break (==':') s

cntGrps :: [Str] -> Acc
cntGrps = Map.fromListWith (+) . fmap parseLine

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
    cntGrps (S.lines body)
        & Map.toAscList
        & map (\(k,v)-> S.unpack k <> ":" <> show v)
        & mapM_ putStrLn 
