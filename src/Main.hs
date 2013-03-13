module Main where

import Categorizer.Wai.Application(application)
import Categorizer.Configuration(Configuration(..), loadConfiguration)
import Categorizer.Text.Matcher(loadMatcher)
import Network.Wai.Handler.Warp(run)
import Network.Wai.Middleware.Static(only, addBase, (<|>), Policy, staticPolicy)

main :: IO ()
main = do
    c <- loadConfiguration
    m <- loadMatcher 

    run (_port c) (staticPolicy policy (application m))
    where
        policy :: Policy
        policy = only [("", "static/index.html"), ("/", "static/index.html")] 
                <|> addBase "static"
