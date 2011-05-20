-- Linewise charset conversion for content that has some lines in
-- CP1252/ISO-8859-1 and some lines in UTF-8

module LineConv where

import Control.Applicative
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Data.Encoding as Enc

main = BS8.interact $ conv encsIn encOut
  where
    encsIn = Enc.encodingFromString <$> ["UTF-8", "CP1252"]
    encOut = Enc.encodingFromString "UTF-8"

conv :: Enc.Encoding e => [e] -> e -> BS.ByteString -> BS.ByteString
conv encsIn encOut = BS8.unlines
                   . map (Enc.encodeLazyByteString encOut)
                   . map (either error id . decode encsIn)
                   . BS8.lines

decode :: Enc.Encoding e => [e] -> BS.ByteString -> Either String String
decode encs text = modError text
                 . foldl1 fallback
                 . map (`Enc.decodeLazyByteStringExplicit` text)
                 $ encs
  where
    modError _    (Right a) = Right a
    modError text (Left e)  =
      Left $ "Failed to decode " ++ show text ++ ": " ++ show e

    Left _      `fallback` e = e
    e@(Right _) `fallback` _ = e
