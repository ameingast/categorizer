module Categorizer.Text.Data where

import Data.Aeson

import qualified Data.ByteString as BS

type UUID = BS.ByteString
type Language = BS.ByteString

data Dictionary = Dictionary
    { _dictionaryId :: UUID 
    , _dictionaryName :: BS.ByteString
    , _dictionaryConcepts :: [Concept] }
    deriving (Show, Eq, Read, Ord)

instance ToJSON Dictionary where
    toJSON (Dictionary someId name concepts) = object
        [ "id" .= someId
        , "name" .= name 
        , "concepts" .= toJSON concepts ]

data Concept = Concept
    { _conceptId :: UUID 
    , _conceptPrefLabel :: Label
    , _conceptAltLabels :: [Label]
    , _conceptHiddenLabels :: [Label] }
    deriving (Show, Eq, Read, Ord)

instance ToJSON Concept where
    toJSON (Concept someId prefLabel altLabels hiddenLabels) = object
        [ "id" .= someId
        , "prefLabel" .= toJSON prefLabel
        , "altLabels" .= toJSON altLabels
        , "hiddenLabels" .= toJSON hiddenLabels ]

data Label = Label
    { _labelName :: BS.ByteString
    , _labelLanguage :: Language }
    deriving (Show, Eq, Read, Ord)

instance ToJSON Label where
    toJSON (Label name language) = object
        [ "name" .= name
        , "language" .= language ]
