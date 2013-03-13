module Categorizer.Text.Parser where

import Categorizer.Text.Data
import Categorizer.Util.Maybe(readSafe)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified System.UUID.V4 as UUID

parseDictionary :: DictionaryName -> BS.ByteString -> IO (Maybe Dictionary)
parseDictionary name s =
    case readSafe (C8.unpack s) of
        Nothing -> do
            putStrLn $ "Cannot parse dictionary: " ++ C8.unpack s
            return Nothing
        Just dict -> do
            someId <- UUID.uuid
            return $ Just $ dict 
                { _dictionaryName = name
                , _dictionaryId = C8.pack $ show someId }


