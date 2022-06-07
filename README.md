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
data State = S1 | S2 deriving (Ord, Eq)
data BinaryAlphabet = Zero | One

transitionFunction :: State -> BinaryAlphabet -> State
transitionFunction S1 Zero = S2
transitionFunction S1 One = S1
transitionFunction S2 Zero = S1
transitionFunction S2 One = S2

parser :: DFA State BinaryAlphabet
parser = DFA {startState=S1, transition=transitionFunction, accept=Data.Set.singleton S1}
```

Mostly pretty straightforward. One thing to notice is that the state class needs to be an instance of `Ord` in order for the code here to automatically process it, since this is needed for verifying membership of a final state in the accept set. Once you implement `Ord` for your state (often by just deriving it as above), the DFA can be used as follows:
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

This particular DFA is fully implemented (including a command-line interface) as [TrivialParser](TrivialParser.hs), so check that out for a full example of DFAs in use. [ParityChecker](ParityChecker.hs) is also a DFA, which checks if a binary number is even.

## Nondeterministic Finite Automata
A [Nondeterministic Finite Automaton (NFA)](https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton) is much like a DFA, only less, you know, deterministic. What's meant by this is that a given state is allowed to have multiple transition paths on encountering a particular character, or even 0, rather than strictly 1. Consequently, the only difference in the formal definition here beyond the existing definition of a DFA is that in this case, the transition function returns a set of possible output states for the given input state and character, instead of a single one.

A string is accepted as long as after the input is fully consumed, any of the potential states the machine could be in is an accept state. NFAs are actually not more powerful than DFAs, it's possible to [convert any NFA to an equivalent DFA](https://en.wikipedia.org/wiki/Powerset_construction), but sometimes you can avoid a lot of extra work by expressing your particular machine nondeterministically. Consider this example from Wikipedia:

![(0|1)* 1 (0|1)^3](https://upload.wikimedia.org/wikipedia/commons/5/59/Relatively_small_NFA.svg)

This NFA recognizes strings in the binary alphabet of any number of characters, followed by a 1, and then exactly three more characters. An equivalent DFA would have several times more states. Let's take a look at the Autom implementation (I'll use ABCD instead of 0123 for states because they make better Haskell identifiers).
```haskell
data State = X | A | B | C | D deriving (Ord, Eq)
data BinaryAlphabet = Zero | One

transitionFunction :: State -> BinaryAlphabet -> Maybe (Data.Set.Set State)
transitionFunction X Zero = Just $ Data.Set.singleton X
transitionFunction X One = Just $ Data.Set.fromList [X, A]
transitionFunction A _ = Just $ Data.Set.singleton B
transitionFunction B _ = Just $ Data.Set.singleton C
transitionFunction C _ = Just $ Data.Set.singleton D
transitionFunction _ _ = Nothing

parser :: NFA State BinaryAlphabet
parser = NFA {startState=X, transition=transitionFunction, accept=Data.Set.singleton D}
```

This one is slightly more complex than the DFA case. Again, we have the requirement of the state needing to be `Ord`, but there are a few other quirks. Mainly, the output of the transition is now not just a set (as mandated by the definition of NFAs), but also a `Maybe`. This is so that the last line of transition, `transitionFunction _ _ = Nothing`, can be written. Typically, in an NFA, you don't need to specify that a particular combination of state and input goes nowhere, you simply omit it, and this is taken as an automatic rejection when encountered. However, when Haskell encounters this, it will throw a non-exhaustive pattern match error and the program will explode, which isn't what we want. So unfortunately, this last line is mandatory. Any combination of state and input that does in fact lead to a set of possible states must then be a `Just <set>`. `NFA`, like `DFA`, instances `Processable`, so the usage interface is the same:
```haskell
process parser [Zero, One, Zero, One, Zero]
=> True
process parser [One, One, One]
=> False
```

I've actually used the NFA to implement something that at least approaches usefulness, a validator of email addresses, so if you want a more complete look at using NFA, [check it out](EmailValidator.hs).
