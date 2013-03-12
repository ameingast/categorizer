module Main where

import Categorizer.Wai.Application(application)
import Categorizer.Configuration(Configuration(..), loadConfiguration)
import Categorizer.Text.Matcher(loadMatcher)
import Network.Wai.Handler.Warp(run)
import Network.Wai.Middleware.Static(staticPolicy, addBase)

main :: IO ()
main = do
    c <- loadConfiguration
    m <- loadMatcher 

    let static = staticPolicy (addBase "static")
    let app = application m

    run (_port c) (static app)
