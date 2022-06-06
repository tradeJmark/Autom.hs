import Autom.PDA
import Autom.Processable
import Autom.Alphabets
import Data.Set(singleton, Set)

data State = P | Q | R deriving (Ord, Eq)
data Stack = A | Z deriving (Ord, Eq)

pdaTransition :: State -> Maybe BinaryAlphabet -> Stack -> Maybe (Set (State, [Stack]))
pdaTransition P (Just Zero) Z = Just $ singleton (P, [A, Z])
pdaTransition P (Just Zero) A = Just $ singleton (P, [A, A])
pdaTransition P Nothing     Z = Just $ singleton (Q, [Z])
pdaTransition P Nothing     A = Just $ singleton (Q, [A])
pdaTransition Q (Just One)  A = Just $ singleton (Q, [])
pdaTransition Q Nothing     Z = Just $ singleton (R, [Z])
pdaTransition _ _ _           = Nothing

pda :: PDA State BinaryAlphabet Stack
pda = PDA {transition=pdaTransition, startState=P, startStack=Z, accept=singleton R}

interpretResults :: Maybe Bool -> [Char]
interpretResults (Just True) = "Match"
interpretResults (Just False) = "No Match"
interpretResults Nothing = "Invalid"

main = interact $ unlines . map (interpretResults . processString pda) . lines