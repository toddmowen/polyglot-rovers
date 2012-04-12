It's about time I wrote a solution in Haskell. But at the outset, I should confess to a couple of biases:

1. I studied a unit of Haskell in university many years ago, and although I subsequently forgot most of the details, Haskell has strongly influenced my idea of what a FP language should look and feel like. (Hence surprise when Scheme lacks pattern matching, or Clojure does not automatically curry functions).

2. I picked up Haskell again a couple of months ago, so I already have some basic experience with it, whereas for a lot of the other languages the Mars Rover solution was basically the first code I wrote.

Bias aside, I've had my share of frustration while learning the language, but overall I'm really enjoying Haskell.


First solution: algebraic data types
------------------------------------

Given that one of my goals was to try to design an interface that resembled the problem specification as closely as possible, an obvious starting point in Haskell is to define the bearings (N, S, E, W) and commands (L, R, M) as type constructors, if only because that's the only way to define symbols that start with an uppercase letter! This means we can construct a rover like this:

    Rover 1 2 N

And write a sequence of commands like this:

    [L,M,L,M,L,M,L,M,M]

I wrote a function that applies commands to a Rover, called `exec`. Using infix notation, I could simply write:

    Rover 1 2 N `exec` [L,M,L,M,L,M,L,M,M]
