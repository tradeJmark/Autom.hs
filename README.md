# Autom.hs
Have you ever been writing your Haskell code and though, wow, if only I had a state machine? No? Well, anyway, here's some code for implementing state machines in Haskell.

## Deterministic Finite Automata
A [Deterministic Finite Automaton (DFA)](https://en.wikipedia.org/wiki/DFSA) is a state machine characterized by a finite number of states and exactly one possible move from a given state to another (or the same) state upon encountering a particular input character. Formally, a DFA has five components:

- A set of possible states;
- An alphabet of possible input characters;
- A transition function which, given a current state and input character, returns a new state to move to;
- A state to start from, and
- A set of "accept" states which, if ended on, produce a positive result.

Autom models the first two aspects as data types, the transition function as a function (go figure), the start state as a field on a DFA instance, and the accepting set as a `Data.Set` (again, go figure). So, let us observe the following DFA from Wikipedia:

![Even Zeroes DFA](https://upload.wikimedia.org/wikipedia/commons/9/9d/DFAexample.svg)

This DFA accepts input strings that have an even number of '0' characters (zero is counted as even for this purpose). By the framework above, it can be described this way:

- States: `{S1, S2}`
- Alphabet: `{0, 1}`
- Transition: Already better described in the image than I can in words, but basically a 1 (which has no effect on the number of 0 characters) will just return the machine to the same state it's already in, while a 0 will flip the state.
- Start state: `S1`, which serves as the state representing "Even".
- Accept states: `{S1}`, as this machine is meant to accept strings with even numbers of 0s (changing to a machine that would accept strings with an odd number of 0s is as easy as leaving everything else the same and changing this to `{S2}`).

In Autom, we can represent that DFA like this:
```haskell
data State = S1 | S2
data BinaryAlphabet = Zero | One

transitionFunction :: State -> BinaryAlphabet -> State
transitionFunction S1 Zero = S2
transitionFunction S1 One = S1
transitionFunction S2 Zero = S1
transitionFunction S2 One = S2

parser :: DFA State BinaryAlphabet
parser = DFA {startState=S1, transition=transitionFunction, accept=Data.Set.singleton S1}
```

From there, it can be used as follows:
```haskell
process parser [Zero One One Zero]
=> True
process parser [Zero One Zero Zero One]
=> False
```

It's worth noting that the transition function could be compressed, like this:
```haskell
transitionFunction s One = s
transitionFunction S1 Zero = S2
transitionFunction S2 Zero = S1
```

Reasonable people can disagree on whether or not this is a subversion of the purpose of a DFA, but in general I tend to think it's fine to avoid listing out every single possible transition as long as you aren't, like, doing computation within the Haskell code. Like, you could theoretically do this:
```haskell
transitionFunction :: Int -> Int -> Int
transitionFunction = (+)

adder :: DFA Int Int
adder = DFA {startState=0, transition=transitionFunction, accept=Data.Set.singleton 12}
```

Congratulations, we've implemented extremely limited addition for the exclusive purpose of determining whether or not a string of numbers sums to 12, by using the Haskell mechanism of addition and then adding in a whole bunch of extra steps. You can do it if you want (I won't stop you), but I don't see the point when this would be more expressive:
```haskell
twelveCheck :: [Int] -> Bool
twelveCheck ints = (sum ints) == 12
```

This particular DFA is fully implemented (including a command-line interface) as [TrivialParser.hs](TrivialParser.hs), so check that out for a full example of DFAs in use.
