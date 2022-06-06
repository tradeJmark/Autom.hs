{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, NamedFieldPuns #-}
module Autom.PDA(PDA(..), process) where

import qualified Data.Set as S
import Data.List(foldl')
import Autom.Processable
import Data.Maybe(fromMaybe)

type Status s sa = (s, [sa])

data PDA s ia sa = PDA {
    transition :: s -> Maybe ia -> sa -> Maybe (S.Set (Status s sa)),
    startState :: s,
    startStack :: sa,
    accept :: S.Set s
}
instance (Ord s, Ord sa) => Processable (PDA s is sa) is where
    process pda@PDA{startState, startStack, accept} str =
        let startSet = S.singleton (startState, [startStack])
            expanded = \statuses -> epsilonExpand pda statuses
            ends = S.map fst $ expanded $ foldl' (\poss c -> foldMap (safeTransition pda (Just c)) (expanded poss)) startSet str
        in not $ ends `S.disjoint` accept

epsilonExpand :: (Ord s, Ord sa) => PDA s ia sa -> S.Set (Status s sa) -> S.Set (Status s sa)
epsilonExpand pda statuses 
    | S.null statuses = S.empty
    | otherwise = statuses `S.union` foldMap (epsilonExpand pda . safeTransition pda Nothing) statuses

pushStack :: [sa] -> Status s sa -> Status s sa
pushStack st (state, new) = (state, new ++ st)

safeTransition :: (Ord s, Ord sa) => PDA s ia sa -> Maybe ia -> Status s sa -> S.Set (Status s sa)
safeTransition _ _ (_, []) = S.empty
safeTransition PDA{transition} c (state, (sc:st)) =
    S.map (pushStack st) . fromMaybe S.empty $ transition state c sc
    