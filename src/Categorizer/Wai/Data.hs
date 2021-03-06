module Categorizer.Wai.Data where

import Categorizer.Text.Data
import Categorizer.Text.Document
import Data.Aeson

import qualified Data.ByteString as BS

data CategorizerRequest
    = AddDictionaryRequest
        { _addDictionaryName :: BS.ByteString
        , _addDictionaryText :: BS.ByteString }
    | GetDictionaryRequest
        { _getDictionaryId :: UUID }
    | ListDictionaryRequest
    | ExtractRequest
        { _extractId :: BS.ByteString
        , _extractText:: BS.ByteString
        , _extractLanguage :: BS.ByteString }
    | InvalidRequest
    deriving (Show, Eq, Read, Ord)

data CategorizerResponse
    = AddDictionaryResponse
        { _addDictionaryResponseId :: UUID }
    | GetDictionaryResponse
        { _getDictionaryResponseDictionary :: Maybe Dictionary }
    | ListDictionaryResponse
        { _listDictionaryResponse :: [Dictionary]}
    | ExtractResponse
        { _extractResponseConcepts :: Document }
    | ErrorResponse
        { _errrorResponseReason :: BS.ByteString }
    deriving (Show, Eq, Read, Ord)

instance ToJSON CategorizerResponse where
    toJSON (AddDictionaryResponse someId) = object
        [ "id" .= someId ]
    toJSON (GetDictionaryResponse (Just dict)) = object
        [ "dictionary" .= toJSON dict ]
    toJSON (GetDictionaryResponse _) = toJSON
        (ErrorResponse "No Such dictionary")
    toJSON (ListDictionaryResponse dicts) = object
        [ "dictionaries" .= toJSON dicts ]
    toJSON (ExtractResponse concepts) = object
        [ "extraction" .= toJSON concepts ]
    toJSON (ErrorResponse reason) = object
        [ "error" .= reason ]
