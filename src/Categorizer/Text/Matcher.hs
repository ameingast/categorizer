module Categorizer.Text.Matcher where

import Categorizer.Util.List(safeHead)
import Categorizer.Util.Maybe(readSafe)
import Categorizer.Text.Data
import Categorizer.Text.Document
import Categorizer.Util.IO(readFileSafe)
import Control.Concurrent.MVar
import Control.Monad(liftM)
import Data.List

import qualified Data.ByteString as BS
import qualified Data.Map as M

data InMemoryMatcher = InMemoryMatcher
    { _matcherDictionaries :: MVar [Dictionary] }
    deriving (Eq)

showMatcher :: InMemoryMatcher -> IO String
showMatcher matcher = liftM show (readMVar $ _matcherDictionaries matcher)

loadMatcher :: IO InMemoryMatcher
loadMatcher =
    loadMatcherFromFile "data/dict.txt" >>= \m -> case m of
        Nothing -> do
            putStrLn "Creating new dictionary list"
            newMatcher
        Just matcher -> do
            putStrLn "Loaded dictionaries" 
            return matcher
    where
        loadMatcherFromFile :: FilePath -> IO (Maybe InMemoryMatcher)
        loadMatcherFromFile file = readFileSafe file >>= \f -> case f of
            Nothing -> return Nothing
            Just contents -> readMatcher contents

        readMatcher :: String -> IO (Maybe InMemoryMatcher)
        readMatcher s = case readSafe s of
            Nothing -> return Nothing
            (Just dicts) -> liftM (Just . InMemoryMatcher) (newMVar dicts)

        newMatcher :: IO InMemoryMatcher
        newMatcher = liftM InMemoryMatcher (newMVar [])

addDictionary :: InMemoryMatcher -> Dictionary -> IO UUID
addDictionary matcher dict = modifyMVar
    (_matcherDictionaries matcher)
    (\ds -> return (dict:ds, _dictionaryId dict))

getDictionary :: InMemoryMatcher -> UUID -> IO (Maybe Dictionary)
getDictionary matcher dictId = liftM
    (safeHead . filter (\d -> dictId == _dictionaryId d))
    (readMVar $ _matcherDictionaries matcher)

listDictionaries :: InMemoryMatcher -> IO [Dictionary]
listDictionaries = readMVar . _matcherDictionaries

extract :: InMemoryMatcher -> UUID -> BS.ByteString -> Language -> IO (Maybe Document)
extract matcher dictId text language =
    getDictionary matcher dictId >>= \d -> case d of
        Nothing -> return Nothing
        Just dict -> return $ Just $ matchAnnotations dict (parseDocument text language)

matchAnnotations :: Dictionary -> Document -> Document
matchAnnotations dict doc = doc 
    { _documentAnnotations = annotations wordToPosIdx [] }
    where
        wordToPosIdx = (M.toList . _idxPosToWord . _documentWordIndex) doc
        concepts = _dictionaryConcepts dict
        language = _documentLanguage doc

        annotations :: [(Integer, Word)] -> [Annotation] -> [Annotation]
        annotations [] as = as
        annotations ((pos, word):xs) as = 
            let cs = lookupConcepts word concepts []
            in annotations xs (if null cs then as else ConceptAnnotation cs pos:as)

        lookupConcepts:: Word -> [Concept] -> [Concept] -> [Concept] 
        lookupConcepts _ [] mcs = mcs
        lookupConcepts w (c:cs) mcs | matchesConcept w c = 
            lookupConcepts w cs (c:mcs)
        lookupConcepts w (_:cs) mcs = lookupConcepts w cs mcs

        matchesConcept :: Word -> Concept -> Bool
        matchesConcept w c = 
            matchesLabel w (_conceptPrefLabel c) ||
            matchesLabels w (_conceptAltLabels c) ||
            matchesLabels w (_conceptHiddenLabels c)

        matchesLabel :: Word -> Label -> Bool
        matchesLabel w (Label name lang) = w == name && lang == language

        matchesLabels :: Word -> [Label] -> Bool
        matchesLabels w ls = case find (matchesLabel w) ls of
            Nothing -> False
            _ -> True
