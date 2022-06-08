import Autom.NFA
import Data.Set(singleton, fromList, Set)

data EmailState = Init | Name | At | Domain | Dot | TLD deriving (Ord, Eq)

isLetter :: Char -> Bool
isLetter c
    | c >= 'a' && c <= 'z' = True
    | c >= 'A' && c <= 'Z' = True
    | otherwise            = False

emailTransition :: EmailState -> Char -> Maybe (Set EmailState)
emailTransition Init c | isLetter c = Just $ singleton Name
emailTransition Name c
    | isLetter c = Just $ singleton Name
    | c == '.'   = Just $ singleton Name
    | c == '@'   = Just $ singleton At
emailTransition At c | isLetter c = Just $ singleton Domain
emailTransition Domain c
    | isLetter c = Just $ singleton Domain
    | c == '.'   = Just $ fromList [Domain, Dot]
emailTransition Dot c | isLetter c = Just $ singleton TLD
emailTransition TLD c | isLetter c = Just $ singleton TLD
emailTransition _ _ = Nothing

emailValidator :: NFA EmailState Char
emailValidator = NFA{startState=Init, transition=emailTransition, accept=singleton TLD}

interpretResults :: Bool -> [Char]
interpretResults True = "Valid"
interpretResults False = "Invalid"

main = interact $ unlines . map (interpretResults . process emailValidator) . lines
