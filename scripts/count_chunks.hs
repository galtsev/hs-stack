
import Conduit
import qualified Data.ByteString as BS

data Stats = Stats {
    sMin :: Double,
    sMax :: Double,
    sAvg :: Double,
    sStdDev :: Double
} deriving (Show, Eq)

totalLen :: ConduitT () Void IO Int
totalLen = stdinC .| foldlC (\acc ln -> acc + BS.length ln) 0

countChunks :: ConduitT () Void IO Int
countChunks = stdinC .| foldlC (\acc _ -> acc + 1) 0

statChunks :: ConduitT () Void IO Stats
statChunks = fmap stat pipe
    where
        pipe = stdinC .| mapC BS.length .| sinkList
        stat :: [Int] -> Stats
        stat l = Stats (minimum lx) (maximum lx) avg sd
            where
                lx = map fromIntegral l
                len :: Double
                len = fromIntegral . length $ l
                avg :: Double
                avg = sum lx / len
                sqr :: Double -> Double
                sqr x = x * x
                sd :: Double
                sd = sqrt $ (sum $ map (sqr . (avg-)) lx) / len


main = runConduit go >>= print
    where
        -- go = totalLen
        -- go = countChunks
        go = statChunks