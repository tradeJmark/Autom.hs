{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, NamedFieldPuns #-}
module  Autom.NFA(NFA(..), process) where
import Data.Foldable(foldl')
import qualified Data.Set as S
import Autom.Processable
import Data.Maybe(fromMaybe)

data NFA s a = NFA {
    transition :: s -> a -> Maybe (S.Set s),
    startState :: s,
    accept :: S.Set s
}
instance (Ord s) => Processable (NFA s a) a where
    process NFA {transition, startState, accept} str =
        let startSet = S.singleton startState
            safeTransition = (\c state -> fromMaybe S.empty $ transition state c)
            ends = foldl' (\poss c -> foldMap (safeTransition c) poss) startSet str
        in not $ ends `S.disjoint` accept