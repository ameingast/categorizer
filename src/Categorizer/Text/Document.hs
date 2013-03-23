module Categorizer.Text.Document where

import Data.Aeson
import Categorizer.Text.Data
import Categorizer.Text.Tokenizer

import qualified Data.ByteString as BS
import qualified Data.Map as M

data WordIndex = WordIndex
    { _idxPosToWord :: M.Map Integer Word
    , _idxWordToPos :: M.Map Word [Integer] }
    deriving (Show, Eq, Read, Ord)

emptyWordIndex :: WordIndex
emptyWordIndex = WordIndex M.empty M.empty

instance ToJSON WordIndex where
    toJSON (WordIndex _posToWord wordToPos) = toJSON wordToPos

data Annotation
    = ConceptAnnotation 
        { _annotationConcepts :: [Concept]
        , _annotationPos :: Integer }
    | EmptyAnnotation
    deriving (Show, Eq, Read, Ord)

instance ToJSON Annotation where
    toJSON (ConceptAnnotation concepts pos) = object 
        [ "type" .= toJSON ("concept" :: String)
        , "concepts" .= concepts
        , "pos" .= pos ]
    toJSON (EmptyAnnotation) = object []

data Document = Document
    { _documentWordIndex :: WordIndex
    , _documentLanguage :: Language 
    , _documentAnnotations :: [Annotation] }
    deriving (Show, Eq, Read, Ord)

instance ToJSON Document where
    toJSON (Document wordIndex language annotations) = object
        [ "wordIndex" .= wordIndex
        , "language" .= language
        , "annotations" .= annotations ]

parseDocument :: BS.ByteString -> Language -> Document
parseDocument s language = Document 
    { _documentWordIndex = buildWordIndex 1 emptyWordIndex (tokenizeWords makeTokenizer s)
    , _documentLanguage = language
    , _documentAnnotations = [] }

buildWordIndex :: Integer -> WordIndex -> [BS.ByteString] -> WordIndex
buildWordIndex _ idx [] = calculateWordToPos idx
buildWordIndex pos idx (x:xs) = 
    let newIdx = idx { _idxPosToWord = M.insert pos x (_idxPosToWord idx) }
    in buildWordIndex (pos + 1) newIdx xs

calculateWordToPos :: WordIndex -> WordIndex
calculateWordToPos idx = idx { _idxWordToPos = wordToPosIdx }
    where
        wordToPosIdx = calculateWordToPos' (M.toList (_idxPosToWord idx)) M.empty

        calculateWordToPos' :: [(Integer, Word)] -> M.Map Word [Integer] -> M.Map Word [Integer]
        calculateWordToPos' [] m = m
        calculateWordToPos' ((pos, word):xs) m = case M.lookup word m of
            Nothing -> 
                calculateWordToPos' xs (M.insert word [pos] m)
            Just positions ->
                calculateWordToPos' xs (M.insert word (pos:positions) m)

