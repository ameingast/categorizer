module Categorizer.Wai.Application where

import Categorizer.Wai.Data
import Categorizer.Text.Matcher
import Categorizer.Text.Parser(parseDictionary)
import Network.Wai
import Network.Wai.Parse
import Data.Maybe(fromMaybe)
import Data.Aeson
import Network.HTTP.Types
import Control.Monad(liftM)
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource(ResourceT)
import Control.Arrow(second)

import qualified Data.ByteString as BS
import qualified Data.Text as Text

application :: InMemoryMatcher -> Application
application matcher request = do
    params <- liftM (++ queryString request) (postParams request)

    case buildRequest params request of
        (AddDictionaryRequest name text) ->
            liftIO $ parseDictionary name text >>= \r -> case r of
                Nothing ->
                    sendJSONError (ErrorResponse "Invalid dictionary")
                Just dict ->
                    liftIO (addDictionary matcher dict) >>= sendJSON . AddDictionaryResponse
        (GetDictionaryRequest someId) ->
            liftIO (getDictionary matcher someId) >>= sendJSON . GetDictionaryResponse
        ListDictionaryRequest ->
            liftIO (listDictionaries matcher) >>= sendJSON . ListDictionaryResponse 
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

buildRequest :: [(BS.ByteString, Maybe BS.ByteString)] -> Request -> CategorizerRequest
buildRequest params request = case urlRoot request of
    (Just "add") ->
        fromMaybe InvalidRequest $ do
            text <- param "text" params
            name <- param "name" params
            return AddDictionaryRequest
                { _addDictionaryName = name
                , _addDictionaryText = text }
    (Just "get") ->
        fromMaybe InvalidRequest $ liftM GetDictionaryRequest (param "id" params)
    (Just "list") ->
        ListDictionaryRequest
    (Just "extract") ->
        fromMaybe InvalidRequest $ do
            someId <- param "id" params
            text <- param "text" params
            language <- param "language" params
            return ExtractRequest
                { _extractId = someId
                , _extractText = text
                , _extractLanguage = language }
    _  ->
        InvalidRequest

param :: BS.ByteString -> [(BS.ByteString, Maybe BS.ByteString)] -> Maybe BS.ByteString
param _ [] = Nothing
param field ((key, value):_) | key == field = value
param field (_:xs) = param field xs

postParams :: Request -> ResourceT IO [(BS.ByteString, Maybe BS.ByteString)]
postParams request =
    liftM (map (second Just) . fst) (parseRequestBody lbsBackEnd request)

urlRoot :: Request -> Maybe String
urlRoot request =
    case pathInfo request of
        [] -> Nothing
        (root:_) -> (Just . Text.unpack) root
