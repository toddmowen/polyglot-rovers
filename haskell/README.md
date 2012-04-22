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



Third solution: state monad
---------------------------

Just for fun, I set out to write a solution involving "gratuitous" use of a monad. I was mildly surprised to find this actually resulted in quite a clear and concise implementation. But of course, that's exactly what a good abstraction should provide, and monads are a very powerful abstraction.

Specifically, I wrapped the rover's current position and direction in a `State` monad, and implemented the rover commands as operations that modify the state. At first glance, this may seem like a retreat into imperative programming (and proof of the saying that "one can write C in any language"). In defense, I will point out that sequential execution of rover commands is explicit in the problem description, so modeling the problem in this way is actually a very natural mapping of the domain.

To find a rover's final position, we call this `runRover` function with a initial state and an operation. For example, to execute the "M" command:

    runRover (Rover 1 2 'N') m

Multiple actions can be composed into a sequence in a few different ways, for example using a `do` block:

    runRover (Rover 1 2 'N') $
        do m
           r
           m

I rounded off this solution by writing the input parser using the Parsec library -- which is also based on monads.

It's worth noting that, after spending a few months playing with Haskell, I have only recently become familiar enough with monads to even imagine this solution. Most of the difficulty of learning Haskell (but also most of the fun) lies not in the basic syntax, but in grasping the ramifications of the computational model (lazy evaluation) and coming to terms with the standard libraries and the idioms and types which they use. Monads deserve a special mention here. At first they seemed very esoteric to me, something clever but without an obvious use. Yet now I would go so far as to say that you can write Haskell programs without monads, only in the same sense that you can write C++ programs without classes.
