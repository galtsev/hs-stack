
import System.IO (stdout)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.ByteString.Builder (Builder, hPutBuilder, toLazyByteString, byteString, intDec, char8)

type Str = BS.ByteString

groupPrefix :: Builder
groupPrefix = byteString $ BS.pack "long.and.lone.line.prefix."

groupSuffix :: Builder
groupSuffix = byteString $ BS.pack ".suffix"

mkLine :: Int -> Builder
mkLine i = groupPrefix
    <> intDec (mod i 17)
    <> groupSuffix
    <> char8 ':'
    <> intDec (mod i 23)
    <> char8 '\n'

--main = mapM_ (hPutBuilder stdout) $ map mkLine [1..1000000]
main = LBS.writeFile "data.txt" $ toLazyByteString (foldMap mkLine [1..10*1000*1000])