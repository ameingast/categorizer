module Categorizer.Util.IO 
    ( readFileSafe
    , safeGetEnv
    , safeReadEnv ) where

import Control.Monad(liftM)
import Categorizer.Util.Maybe(readSafe)
import System.Environment(getEnv)

import qualified Control.Exception as E(catch, IOException)

readFileSafe :: FilePath -> IO (Maybe String)
readFileSafe path =
    liftM Just (readFile path) `E.catch` handler
    where
        handler :: E.IOException -> IO (Maybe String)
        handler e = print e >> return Nothing

safeGetEnv :: String -> IO (Maybe String)
safeGetEnv s = liftM Just (getEnv s) `E.catch` handler
    where
        handler :: IOError -> IO (Maybe String)
        handler e = print e >> return Nothing

safeReadEnv :: Read a => String -> IO (Maybe a)
safeReadEnv s =
    safeGetEnv s >>= \x -> case x of
        Nothing -> return Nothing
        Just p -> (return . readSafe) p
