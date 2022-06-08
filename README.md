# Autom.hs
Have you ever been writing your Haskell code and though, wow, if only I had a state machine? No? Well, anyway, here's some code for implementing state machines in Haskell.

## Deterministic Finite Automata
A [Deterministic Finite Automaton (DFA)](https://en.wikipedia.org/wiki/DFSA) is a state machine characterized by a finite number of states and exactly one possible move from a given state to another (or the same) state upon encountering a particular input character. They can recognize the category of laguages known as [regular languages](https://en.wikipedia.org/wiki/Regular_language) Formally, a DFA has five components:

- A set of possible states;
- An alphabet of possible input characters;
- A transition function which, given a current state and input character, returns a new state to move to;
- A state to start from; and
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

I've actually used the NFA to implement something that at least approaches usefulness, a validator of email addresses (albeit an overly restrictive one), so if you want a more complete look at using NFA, [check it out](EmailValidator.hs).

## Pushdown Automata
A [Pushdown Automaton (PDA)](https://en.wikipedia.org/wiki/Pushdown_automaton) is similar to the above finite state machines, only it also has a stack. PDAs can conceptually be either deterministic or not, but the Autom implementation is nondeterministic, so it will be similar to the NFA above, but with a stack. The stack makes it more powerful than the other two machine types, and it can recognize the broader class of [context-free languages](https://en.wikipedia.org/wiki/Context-free_languages). Here's the seven components of the formal description:
- Again, a set of states;
- As above, an input alphabet;
- A set of characters that can go on the stack, known as the stack alphabet;
- A transition function, although this one is a little more complicated, because the output is not just a state, but additionally, a string of stack alphabet symbols to be pushed onto the stack;
- A start state;
- A stack symbol that starts at the head of the stack; and
- A set of accept states, just like above.

In Autom, the parts that are shared with the finite state machines above are implemented in the same way, and additionally, the stack alphabet is a data type (like the input alphabet), and the start stack symbol is a field on a PDA instance. Let's look at an example PDA (thanks yet again, Wikipedia):

![0^n1^n](https://upload.wikimedia.org/wikipedia/commons/thumb/3/3c/Pda-example.svg/2560px-Pda-example.svg.png)

This automaton recognizes strings in the binary alphabet composed of an arbitrary number of 0s, followed by _that same number_ of 1s. This would not have been possible to recognize with a finite state machine, the stack is what enables it. The constructions that look like `Z/AZ` mean that this path can be taken when the top of the stack is `Z`, and after `Z` is popped off the stack, it is to be replaced with `AZ` being pushed on the stack. The transitions where the character is <code>&epsilon;</code> (epsilon transitions) means that the machine can take this transition without needing to actually consume a character. I think it will help to go through an execution of his machine in order to understand how it works, since it's a little more complex than the finite state machines. Let's use the string 0011.

- At the start, we are on state `p`, and on the stack is a `Z` (the image doesn't really show this, but I'm telling it to you now). This is the status (I don't think "status" is a term of art here, it's just what I use to describe the combination of a state and the stack frozen at a point in time) which I will refer to with the notation `p[Z]`. However, since the machine can take an epsilon transition to `q` and further to `r` (note that it's always possible to go freely from `p` to `q`, but it's only currently possible to go to `r` because of a `Z` showing at the top of the stack), the actual status we might have before consuming any character is `p[Z] | q[Z] | r[Z]`.
- Now, we consume a 0. From `p[Z]`, there are two transitions we can take: `0; Z/AZ` back to `p`, which lands us at `p[AZ]`, or <code>&epsilon;</code> to `q`. We can ignore that, and all other epsilon moves for that matter, because we already dealt with that when we expended our set at the end of last step. From `q[Z]`, since there are no 0 moves, we can't go anywhere, and the same is true for `r[Z]`, as you can never go anywhere starting from there. So, after consuming the 0, our status is certainly `p[AZ]`. Let's expand that again by epsilon moves before consuming anything else: we can still take the path to `q`, but with no `Z` at the top of the stack, we can't go from `q` to `r`. So, our status at the moment is either `p[AZ]` or `q[AZ]`.
- Now we consume the second 0. From `p[AZ]`, we can take `0; A/AA`, which pops the `A` from the top of the stack and adds in two more, leaving us with a possible status of `p[AAZ]`. We again have no possible move to make from `q[AZ]`, so our only potential state before expansion is `p[AAZ]`. Looking at epsilon moves we can make, again the only option is the one from `p` to `q`. That adds `q[AAZ]` as a potential status.
- Now we consume a 1. There are no 1 moves from `p`, so now that's a dead end. We can finally take the transition from `q` to itself, though. The reason this shows <code>A/&epsilon;</code> is that it adds nothing onto the stack after popping; it is a pure pop. It leaves us at `q[AZ]`. There is no epsilon move to be made from here.
- Consuming the final 1, our only choice now is to take the `q` self-transition again, popping the stack again, leaving us at `q[Z]`. Uh-oh, `q` isn't an accept state. But ah-hah, we have an epsilon move to make this time. We can move, without needing another character, over to the status `r[Z]`, and since `r` is an accpeting state, congratulations, we have a valid string. The moves we took, sequentially, to get to the accept state, were <code>0; Z/AZ -> 0; A/AA -> &epsilon; -> 1; A/&epsilon; -> 1; A/&epsilon; -> &epsilon;; Z/Z</code>.

So now that hopefully you understand how a PDA works, let's look at the Autom version of this one.
```haskell
data State = P | Q | R deriving (Eq, Ord)
data BinaryAlphabet = Zero | One
data StackAlphabet = A | Z deriving (Eq, Ord)

transitionFunction :: State -> Maybe BinaryAlphabet -> StackAlphabet -> Maybe (Set (State, [StackAlphabet]))
transitionFunction P (Just Zero) c = Just $ Data.Set.singleton (P, [A, c])
transitionFunction P Nothing c = Just $ Data.Set.singleton (Q, [c])
transitionFunction Q (Just One) A = Just $ Data.Set.singleton (Q, [])
transitionFunction Q Nothing Z = Just $ Data.Set.singleton (R, [Z])
transitionFunction _ _ _ = Nothing

parser :: PDA State BinaryAlphabet StackAlphabet
parser = PDA{startState=P, transition=transitionFunction, startStack=Z, accept=Data.Set.singleton R}
```

Again, a little more complex as we make a more powerful machine than earlier ones. It has basically the same quirks as NFAs (returning a `Maybe` and needing the `Nothing` line), and a few new ones. First of all, this time the input character is a `Maybe` character now, to accomodate epsilon-moves. `Nothing` represents <code>&epsilon;</code>. There's also a third argument to the transition, since moves in PDAs depend additionally on the character at the top of the stack. The return value (disregarding the `Maybe`) is a set of what I've been calling statuses, although it's more of a partial status, i.e. a new state plus the new characters to add onto the top of the stack (not an entire stack itself). The only other thing probably worth mentioning is that now I'm requiring the stack alphabet type to also instance `Ord`, since it has to be part of a set. Like the other automata, my `PDA` type instances `Processable`, so the usage interface is again the same:
```haskell
process parser [Zero, Zero, Zero, One, One, One]
=> True
process parser [Zero, One, One]
=> False
```

I've fully implemented the PDA we've been discussing as [PDATest](PDATest.hs), so you can check that out for an example.

## Included alphabets
The codebase here includes the type class `Alphabet` in the `Autom.Alphabets` module. You can instance this class with your alphabet, and implement the `parse` method to convert a given `Char` into a symbol from your alphabet (strictly, `Maybe` a symbol, where `Nothing` means the input is invalid for that alphabet). This unlocks the use of `processString`, which takes any `Processable` (so long as its input alphabet is an `Alphabet`) and a string instead of a list of alphabet symbols. You can use it instead of `process` when your input is a string. The binary alphabet we've used in a bunch of above examples, composed of `Zero` and `One`, is included here, and it is an instance of `Alphabet`, so for example using that first DFA from above, we could run this:
```haskell
processString parser "0010101"
=> True
processString parser "010101"
=> False
```

This can be very useful especially for the DFA, because you'll often want as small an alphabet as possible, every symbol needing to have a path from every state. In the nondeterministic machines, this is not as essential, because you can often make use of a significantly larger alphabet than strictly necessary, and then just not have any movements at any point in the machine for characters that you don't want to use. This is the approach taken by [EmailValidator](EmailValidator.hs), which uses `Char` directly as its alphabet and just doesn't use any symbols besides letters, @, and . in its transitions.

## HQs
I've never made something cool enough for people to frequently have questions about it, but I'll answer some hypothetical questions one might have here.

### Q: Why would I ever want to use this?
Well, you probably wouldn't, mostly. Finite Automata can do most of what simple regular expressions can, and PDAs can do most of what the more extended modern regex implementations can do, but usually the regexes will be more convenient. This is kind of like how the concept of a Turing Machine exists and is a useful model for theory, but in practice any machine you'll use will have a von Neumann architecture. These formal state machines are useful to learn about language theory, but you won't often use them in practical applications. When you are learning about them, though, it's often really helpful to be able to implement them for yourself and play around with them. I first did something like this in Scala back in university so that I could check my homework in a course named Computability & Formal Language. If you're in that kind of class you might actually really like this library.

### Q: I think that first question-asker is dumb, and I love this magnificent library. How can I install it?
That's an excellent question. I have no idea. I use Haskell for fun, I've never made a library with it before. If you wanna help me set that up, go right ahead and post in the Issues on this repo. I might eventually look it up on my own, who knows. Until then, I guess clone it and write your code within the root? It's really just a toy.
