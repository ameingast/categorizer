module Categorizer.Rdf where

import Data.RDF
import Data.RDF.TriplesGraph
import Text.RDF.RDF4H.NTriplesParser
import Categorizer.Text.Data
import Control.Monad(liftM)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map as M

-- public function to get the dictionary from an rdf dump
getDictFromRdf :: IO Dictionary
getDictFromRdf = liftM convertGraph graph -- i have no idea what i'm doing.... :D

-- print a list of all distinct predicates
seeDistinctPredicates :: IO ()
seeDistinctPredicates = do
    g <- graph
    putStrLn $ show (pickPredicates [] (triplesOf g))

-- read graph from file, should be replaced to read from url
graph :: IO TriplesGraph
graph = fmap fromEither (parseFile NTriplesParser "static/dump/stw.nt")

-- filter triples for those with objects of label predicates
getLabelTriples :: TriplesGraph -> Triples
getLabelTriples g =
    filter hasLabel (take 50 (triplesOf g))

-- true if a triple's predicate is one of the label predicates
hasLabel :: Triple -> Bool
hasLabel t = elem (predicateOf t) labelPredicates

-- defined predicated to get labels from
labelPredicates :: [Node]
labelPredicates = [
		    UNode("http://www.w3.org/2004/02/skos/core#prefLabel"),
                    UNode("http://www.w3.org/2004/02/skos/core#altLabel"),
		    UNode("http://www.w3.org/2004/02/skos/core#hiddenLabel")
		  ]

-- convert graph to dictionary taking labels defined by label predicates
convertGraph :: TriplesGraph -> Dictionary
convertGraph g = Dictionary 
                    "uuid"
                    "dictName"
                    (pickLangs [] (triplesOf g))
                    (pickConcepts (triplesOf g))
                    
                    
pickConcepts :: Triples -> [Concept]
pickConcepts ts = convertMap (updateConcepts M.empty ts)

convertMap :: M.Map Node Concept -> [Concept]
convertMap m = M.elems m

-- TODO: make end recursive? state monad
-- iterate over triples and update the concept maps for each triple
updateConcepts :: M.Map Node Concept -> Triples -> M.Map Node Concept 
updateConcepts m [] = m
updateConcepts m (t:ts) = updateConcepts (M.alter (updateConcept t) (subjectOf t) m) ts

-- update function for a concept node in the map
updateConcept :: Triple -> Maybe Concept -> Maybe Concept
updateConcept t mc =
        case unodeToUUID (subjectOf t) of
            Just uuid -> Just (updateConceptProperty 
                                (justConcept mc uuid) 
                                (predicateOf t) 
                                (objectOf t))
            Nothing -> Nothing
    
-- 
updateConceptProperty :: Concept -> Node -> Node -> Concept
updateConceptProperty c pred obj =
    case pred of
        UNode("http://www.w3.org/2004/02/skos/core#prefLabel")
            -> c { _conceptPrefLabel = convertToLabel obj}
        UNode("http://www.w3.org/2004/02/skos/core#altLabel")
            -> c {_conceptAltLabels = (convertToLabel obj) : (_conceptAltLabels c) }
        UNode("http://www.w3.org/2004/02/skos/core#hiddenLabel")
            -> c {_conceptHiddenLabels = (convertToLabel obj) : (_conceptHiddenLabels c) }
        otherwise -> c


unodeToUUID :: Node -> Maybe UUID
unodeToUUID (UNode uri) = Just (C8.pack(T.unpack uri))
unodeToUUID _ = Nothing

justConcept :: Maybe Concept -> UUID -> Concept
justConcept Nothing conceptUri = (Concept conceptUri (Label "" "") [] [])
justConcept (Just c) _ = c
                                       
convertToLabel :: Node -> Label
convertToLabel (LNode (PlainLL label lang)) = 
    (Label (C8.pack (T.unpack label)) (C8.pack (T.unpack lang)))


--
-- LANGUAGES
--

-- get language list of triples
pickLangs :: [Language] -> Triples -> [Language]
pickLangs detectedLangs [] = detectedLangs
pickLangs detectedLangs (t:ts) = case pickLang (objectOf t) of
                                     Nothing -> pickLangs detectedLangs ts
                                     (Just lang) -> if lang `elem` detectedLangs
                                                    then pickLangs detectedLangs ts
                                                    else pickLangs (lang : detectedLangs) ts

-- get the language of a LiteralNode
pickLang :: Node -> (Maybe Language)
pickLang (LNode (PlainLL label lang)) = Just (C8.pack (T.unpack lang))
pickLang _ = Nothing


-- get language list of triples
pickPredicates :: [Node] -> Triples -> [Node]
pickPredicates detected [] = detected
pickPredicates detected (t:ts) = 
    let pred = (predicateOf t)
    in
        if pred `elem` detected
            then pickPredicates detected ts
            else pickPredicates (pred : detected) ts

