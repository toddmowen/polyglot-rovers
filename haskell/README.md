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



Second solution: vector arithmetic
----------------------------------

Being such a mathematical language, I almost expected that Haskell might have matrix types in its core library, but this is not the case. In fact, I couldn't even find a third party library that suited my needs. However, it was easy to define my own vector and matrix types and write the operations I needed (just addition and multiplication).

Implementing this bit of "plumbing" turned out to be well worth it, because the rover commands then became concise, mathematical statements. Given that `Rover p v` represents a rover with position vector `p` and velocity vector `v`, the right-turn command `r` is defined as follows:

    rotateRight = ( ( 0, 1)
                  , (-1, 0)
                  )

    r (Rover p v) = Rover p (rotateRight `mult` v)

Due to rules about how functions are named, I had to deviate from the "language" of the problem spec slightly by using lowercase instead of uppercase letters for the bearings and commands. This itself is of no great significance, but did get me thinking about whether it would be easy to embed a more extensive DSL in Haskell. My conclusion: probably not easy! (There are extensions such as quasiquotation, which I haven't looked into, that apparently address this kind of need, but at my current point in the Haskell learning curve I am hardly about to label these as "easy"). On the other hand, the language does give you a lot of power and flexibility in designing abstractions.
