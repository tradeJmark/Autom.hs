import Autom.DFA
import Autom.Alphabets
import Autom.Processable
import Data.Set(singleton)

main = do
    interact $ unlines . map (interpretResult . processString trivialParser) . lines

data State = S1 | S2 deriving(Ord, Eq)

trivialTransition :: State -> BinaryAlphabet -> State
trivialTransition S1 Zero = S2
trivialTransition S1 One = S1
trivialTransition S2 Zero = S1
trivialTransition S2 One = S2

trivialParser :: DFA State BinaryAlphabet
trivialParser = DFA {startState=S1, transition=trivialTransition, accept=singleton S1}

interpretResult :: Maybe Bool -> [Char]
interpretResult (Just True) = "Match"
interpretResult (Just False) = "No Match"
interpretResult Nothing = "Invalid"
