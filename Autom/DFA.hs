{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Autom.DFA(DFA(..), process) where

import Data.Set(Set, member)
import Data.Foldable
import Autom.Processable

data DFA s a = DFA {
    startState :: s,
    transition :: s -> a -> s,
    accept :: Set s
}
instance (Ord s) => Processable (DFA s a) a where
    process DFA {startState=start, transition=t, accept=acc} str = (foldl' t start str) `member` acc
