Initial impressions of Scala, based on quite a bit of reading and only a little bit of playing with the REPL:

Scala seems like a very *rich* language. That's not necessarily a barrier to entry, as one can still write code in a simple Java style with classes, methods and imperative statements. But beyond that, Scala offers many more features, starting of course with functional programming, and also the frequently noted sophistication of the type system.

My other first impression, purely aesthetic, is that I find the syntax rather ugly. It doesn't seem as elegant as Haskell (probably because it has to be functional and imperative all at the same time), and I really don't see why Scala needs to invent so many bewildering symbols ('<-', '=>', '!!', '<:', etc) instead of using a few well-chosen keywords. But having said that, I'm going to try to put that prejudice aside (just like anyone wanting to appreciate the beauty of LISP needs to temporarily suspend any distaste they might have for nested parentheses).


First solution: functional composition
--------------------------------------

My first attempt was a purely functional solution (no side-effects). The only class it defined, Rover, was a one-liner -- more of a data type, really, than a rich class. Operations were all written as functions, rather than methods. This, again, is a common style in functional programming, and in this case it also made it very easy to model a sequence of rover commands using functional composition. Each command had the type:

    (Rover) => Rover

That is, a function that takes a Rover and returns a Rover. Scala allows functions to be combined, to create new functions, for example:

    scala> val halfTurn = L andThen L
    halfTurn: (Rover) => Rover = <function1>

Notice that the type of halfTurn is the same as a single rover command (it takes a Rover and returns a Rover). This is not the only way to solve the problem, but it's kinda *cool*, because it blurs the distinction between code and data. Structurally, a sequence of functions combined in this way resembles a linked list.


Second solution: methods
------------------------

Despite its functional programming goodness, Scala is still an object-oriented language, in which objects and methods abound. I tried implementing all the operations as methods instead of functions, with the result being slightly shorter code because the pattern matching used in the first solution was no longer necessary, but also this slightly odd "train carriage" syntax in tests:

    Rover(1, 2, N).L.M.L.M.L.M.L.M.M

Of course, it would be easy to reproduce the syntax from the first solution, by simply creating functions that invoke methods, e.g.

    def L = (_: Rover).L

Perhaps it just shows I've spent too many years writing object oriented code, but I think there are some advantages to using methods. When programming in the small, functions and methods are more-or-less interchangeable, as the code above shows. But in large systems they make possible better data hiding (using private fields) and encapsulation of behaviour. These are organizational benefits rather than technical benefits, but important nevertheless.


Third solution: actors
----------------------

Just for fun, I wrote a solution using Scala's much-touted actors library. Obviously, this added considerable complexity to the design (and especially to the way the code is actually called, as the tests demonstrate), though in terms of syntax it's still reasonably compact.

This solution assumes that messages are received by the actor in a deterministic order. Clearly, this means it makes no sense for multiple threads to communicate with the actor concurrently. But even with only one calling thread, this assumption may not hold true. Unlike Erlang, which guarantees that messages between two given processes are received in the order that they are sent, Scala apparently doesn't make the same guarantee, and I found a [code snippet][1] on stackoverflow.com which demonstrates this "feature" (though I haven't actually seen it manifested in my code yet).

[1]: http://stackoverflow.com/questions/5751993/why-are-messages-received-by-an-actor-unordered/6093131#6093131


Fourth solution: streams
------------------------

In all the previous solutions, I stopped after implementing an API, and never got around to writing the I/O routines for reading the rover "mission" from a file and printing out the results. I'm sure that if I had, I would have approached it in a stateful way: either in a loop that read, executed, and printed each rover, or by reading the whole file into an intermediate data structure (such as a List), applying the transformation, and then printing the results.

While writing the "clojure/sequences" solution, I struck upon the stream processing pattern as a more "functional" way of dealing with I/O. It allows us to explicitly order certain operations (for example, that the line describing the rover's initial position is read before the list of commands), while remaining agnostic about the order of operations in general (for example, how transformation and output are interleaved with the read operations).

Note on terminology: the kind of data structures we need for this design pattern are called "streams" in Scala (where they are implemented in `scala.collection.immutable.Stream`), and called "lazy sequences" in Clojure. Many of Clojure's core library functions return lazy sequences, and so stream processing tends to be the default in that language, rather than a special case.

This solution is essentially a translation of the "clojure/sequences" solution into Scala, although it differs in some details. Most noticeably, Scala (like Java) encourages the organization of code into classes and methods, whereas Clojure encourages the use of static functions. Also, while the Clojure solution attempted to separate the concerns of parsing the input and performing the transformation, in the Scala solution I leant instead towards the principle of simple design by merging both concerns in the `executeRoverLines` method.
