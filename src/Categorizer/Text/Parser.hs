module Categorizer.Text.Parser where

import Categorizer.Text.Data
import Categorizer.Util.Maybe(readSafe)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8

parseDictionary :: BS.ByteString -> Maybe Dictionary
parseDictionary s = readSafe (C8.unpack s)
