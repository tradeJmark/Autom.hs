{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Autom.Processable(Processable(..), processString) where
import Autom.Alphabets

class Processable p a | p -> a where
    process :: (Foldable f) => p -> f a -> Bool

processString :: (Alphabet a, Processable p a) => p -> [Char] -> Maybe Bool
processString processable str = fmap (process processable) $ parseLine str