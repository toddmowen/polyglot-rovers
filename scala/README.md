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
