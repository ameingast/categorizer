module Categorizer.Wai.Application where

import Categorizer.Wai.Data
import Categorizer.Text.Matcher
import Categorizer.Text.Parser(parseDictionary)
import Network.Wai
import Data.Maybe(fromMaybe)
import Data.Aeson
import Network.HTTP.Types
import Control.Monad(liftM)
import Control.Monad.IO.Class

import qualified Data.ByteString as BS
import qualified Data.Text as Text

application :: InMemoryMatcher -> Application
application matcher request = case buildRequest request of
    (AddDictionaryRequest text) ->
        case parseDictionary text of
            Nothing ->
                sendJSONError (ErrorResponse "Invalid dictionary")
            Just dict ->
                liftIO (addDictionary matcher dict) >>= sendJSON . AddDictionaryResponse
    (GetDictionaryRequest someId) ->
        liftIO (getDictionary matcher someId) >>= sendJSON . GetDictionaryResponse
    (ExtractRequest someId text language) ->
        liftIO (extract matcher someId text language) >>= \d -> case d of
            Nothing ->
                sendJSONError (ErrorResponse "Invalid Dictionary")
            Just doc ->
                sendJSON (ExtractResponse doc)
    InvalidRequest ->
        sendJSONError (ErrorResponse "Invalid Request")
    where
        sendJSON js = return $ responseLBS status200 headers $ encode js
        sendJSONError js = return $ responseLBS status400 headers $ encode js
        headers = [("content-type", "application/json")]

buildRequest :: Request -> CategorizerRequest
buildRequest request = case urlRoot request of
    (Just "add") ->
        fromMaybe InvalidRequest $ liftM AddDictionaryRequest (param request "text")
    (Just "get") ->
        fromMaybe InvalidRequest $ liftM GetDictionaryRequest (param request "id")
    (Just "extract") ->
        fromMaybe InvalidRequest $ do
            someId <- param request "id"
            text <- param request "text"
            language <- param request "language"
            return ExtractRequest
                { _extractId = someId
                , _extractText = text
                , _extractLanguage = language }
    _  ->
        InvalidRequest

param :: Request -> BS.ByteString -> Maybe BS.ByteString
param request field = param' (queryString request)
    where
        param' :: [(BS.ByteString, Maybe BS.ByteString)] -> Maybe BS.ByteString
        param' [] = Nothing
        param' ((key, value):_) | key == field = value
        param' (_:xs) = param' xs

urlRoot :: Request -> Maybe String
urlRoot request =
    case pathInfo request of
        [] -> Nothing
        (root:_) -> (Just . Text.unpack) root
