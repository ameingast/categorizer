module Main where

import Categorizer.Wai.Application(application)
import Categorizer.Configuration
import Categorizer.Text.Matcher
import Network.Wai.Handler.Warp(run)

main :: IO ()
main = do
    c <- loadConfiguration
    m <- loadMatcher 
    run (_port c) (application m)
