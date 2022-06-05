module Autom.Alphabets (Alphabet(..), BinaryAlphabet(..), parseLine) where

class Alphabet a where
    parse :: Char -> Maybe a
data BinaryAlphabet = Zero | One
instance Alphabet BinaryAlphabet where
    parse '0' = Just Zero
    parse '1' = Just One
    parse _ = Nothing
    
instance Alphabet Char where
    parse = Just . id

parseLine :: (Alphabet a) => [Char] -> Maybe [a]
parseLine = foldr (\c ml -> ml >>= \rl -> fmap (\rc -> rc:rl) (parse c)) $ Just []