module Categorizer.Util.Maybe 
    ( readSafe ) where

readSafe :: Read a => String -> Maybe a
readSafe s =
    case reads s of
        [(x, _)] -> Just x
        _ -> Nothing 
