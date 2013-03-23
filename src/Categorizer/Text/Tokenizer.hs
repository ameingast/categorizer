module Categorizer.Text.Tokenizer where

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString as BS

type Word = BS.ByteString

class Tokenizer a where
    tokenize :: a -> Word -> [Word]

data SplittingTokenizer = SplittingTokenizer 
    { splitWords :: String
    , ignoredWords :: [Word] }
    deriving (Eq, Show, Ord, Read)

instance Tokenizer SplittingTokenizer where
    tokenize = tokenizeWords

makeTokenizer  :: SplittingTokenizer
makeTokenizer = SplittingTokenizer 
    { splitWords = " \n\t.:!?" 
    , ignoredWords = [""] }

tokenizeWords :: SplittingTokenizer -> Word -> [Word]
tokenizeWords tok = filterWords tok . C8.splitWith (`elem` splitWords tok)

filterWords :: SplittingTokenizer -> [Word] -> [Word]
filterWords tok = filter (`notElem` ignoredWords tok)
