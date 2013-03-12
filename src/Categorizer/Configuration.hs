module Categorizer.Configuration 
    ( Configuration(..)
    , loadConfiguration 
    , writeConfiguration ) where

import Categorizer.Util.IO(readFileSafe, safeReadEnv)

data Configuration = Configuration
    { _version :: String 
    , _port :: Int }
    deriving (Show, Eq, Read, Ord)

loadConfiguration :: IO Configuration
loadConfiguration =
    readConfiguration "config.hs" >>= \c -> case c of
        Nothing -> addPortFromEnvironment defaultConfiguration
        Just configuration -> addPortFromEnvironment configuration

defaultConfiguration :: Configuration
defaultConfiguration = Configuration 
    { _version = "0.0.1.0"
    , _port = 8080 }

readConfiguration :: FilePath -> IO (Maybe Configuration)
readConfiguration path =
    readFileSafe path >>= \contents -> case contents of
        Nothing -> return Nothing
        Just configuration -> (return . read) configuration

writeConfiguration :: FilePath -> Configuration -> IO ()
writeConfiguration path = writeFile path . show

addPortFromEnvironment :: Configuration -> IO Configuration
addPortFromEnvironment conf = 
    safeReadEnv "PORT" >>= \r -> case r of
        Nothing -> return conf
        Just p -> return conf { _port = p }
