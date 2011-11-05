Clojure deserves special praise for defining modulus arithmetic with negative numbers in the mathematically correct way, i.e. (mod -1 4) returns 3. This is something I took advantage of when writing a `turn` function.


First solution: records
-----------------------

My initial attempt at a solution in Clojure turned out to be a "functional OOP" hybrid, because the first thing I wrote was `defrecord Rover`. This got me quite far. Records are immutable by default, but support the same interface as maps, meaning that given a record `rover`, creating a new record with the y-coordinate incremented by one is as simple as:

    (update-in rover [:y] inc)

However, it was not all plain sailing for me. Some of the learning curve I took on voluntarily, choosing to experiment with macros and multimethods. But more confusing was that my first attempt at writing a mainline fine within the REPL, but from the command-line it didn't prompt for input. It took me a long time to realize that the reason for this was Clojure's lazy evaluation. I was relying on side-effects (read: `println` statements), and needed to add `do` statements to force evaluation. This is definitely a trap for people like me who bring imperative programming habits to Clojure.

One noteworthy thing about this solution is that it unifies the `L`, `R`, and `M` commands with the `N`, `S`, `E`, and `W` directions, making all of them simply functions that take and return a rover object.


Second solution: sequences
--------------------------

On reflection, there was nothing particularly "functional" in the style of my first solution -- the `doseq` in the mainline function smelled a lot like an imperative `for` loop. For my second attempt, I used a different design paradigm: stream processing. This meant using Clojure's lazy sequences, one of the most fundamental parts of the core library.

The resulting mainline was much more satisfying than before, making the "read-process-write" structure of the program quite obvious:

    (defn -main []
      (dorun (map prn-rover (rover-end-states (read-plateau-and-rovers)))))

Data enters the system from `read-plateau-and-rovers`, is transformed by `rover-end-states`, and finally output by `prn-rover`.

It also occurred to me that I could very concisely define the compass directions in terms of the `quarter-turn` function. This is the first time that I've avoided the need to define N, S, E, and W individually, and it has a definite mathematical elegance.
