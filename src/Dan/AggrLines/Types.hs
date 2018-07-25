module Dan.AggrLines.Types (Str, Acc) where

import Data.Map.Strict (Map)
import qualified Data.ByteString.Lazy.Char8 as S

type Str = S.ByteString
type Acc = Map Str Int
