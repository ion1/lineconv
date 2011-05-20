-- Linewise charset conversion for content that has some lines in
-- CP1252/ISO-8859-1 and some lines in UTF-8

module LineConv where

import Control.Applicative
import Control.Arrow
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Data.Encoding as Enc
import Data.Functor.Alt

main = BS8.interact $ conv encsIn encOut
  where
    encsIn = Enc.encodingFromString <$> ["UTF-8", "CP1252"]
    encOut = Enc.encodingFromString "UTF-8"

conv :: Enc.Encoding e => [e] -> e -> BS.ByteString -> BS.ByteString
conv encsIn encOut  =  BS8.lines
                   >>> map (either error id . decode encsIn)
                   >>> map (Enc.encodeLazyByteString encOut)
                   >>> BS8.unlines

decode :: Enc.Encoding e => [e] -> BS.ByteString -> Either String String
decode encs text  =  map (\enc -> Enc.decodeLazyByteStringExplicit enc text)
                 >>> foldl1 (<!>)  -- Pick the first successful one
                 >>> left (\err -> "Failed to decode " ++ show text
                                ++ ": " ++ show err)
                  $  encs
