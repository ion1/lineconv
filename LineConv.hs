-- Linewise charset conversion for content that has some lines in
-- CP1252/ISO-8859-1 and some lines in UTF-8

module Main (main) where

import qualified Codec.Text.IConv as IC
import Control.Arrow
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS8
import Data.Functor.Alt

main :: IO ()
main = BS8.interact (convert encsIn encOut)
  where
    encsIn = ["UTF-8", "CP1252"]
    encOut = "UTF-8"

convert :: [IC.EncodingName] -> IC.EncodingName -> BS.ByteString
        -> BS.ByteString
convert encsIn encOut  =  BS8.lines
                      >>> map (either error id . convertLine encsIn encOut)
                      >>> BS8.unlines

convertLine :: [IC.EncodingName] -> IC.EncodingName -> BS.ByteString
            -> Either String BS.ByteString
convertLine encsIn encOut text = tryEncs encsIn
  where
    tryEncs  =  map (\enc -> swapE (IC.convertStrictly enc encOut text))
            >>> foldl1 (<!>)  -- Pick the first successful one
            >>> left (\err -> "Failed to decode " ++ show text
                           ++ ": " ++ show (IC.reportConversionError err))

    swapE = either Right Left
