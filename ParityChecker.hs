import Autom.DFA
import Autom.Processable
import Autom.Alphabets
import Data.Set(singleton)

data ParityState = Even | Odd deriving (Ord, Eq)

parityTransition :: ParityState -> BinaryAlphabet -> ParityState
parityTransition _ Zero = Even
parityTransition _ One =  Odd

parityChecker :: DFA ParityState BinaryAlphabet
parityChecker = DFA{startState=Even, transition=parityTransition, accept=singleton Even}

interpretResults :: Maybe Bool -> [Char]
interpretResults (Just True) = "Even"
interpretResults (Just False) = "Odd"
interpretResults Nothing = "Invalid"

main = interact $ unlines . map (interpretResults . processString parityChecker) . lines