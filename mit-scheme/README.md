This is an implementation in [MIT/GNU Scheme](http://www.gnu.org/software/mit-scheme/). The reason I chose this particular dialect was that I was reading [Structure and Interpretation of Computer Programs](http://mitpress.mit.edu/sicp/) at the time, and this is the language used in the book.

Unfortunately, I came to regret choosing this dialect when I tried to write the _M_, _L_, and _R_ functions. My implementation of these turned out to be astonishingly verbose, because after defining a _rover_ structure to represent the current position and bearing of a rover, I then had to use something like the following just to access the x-coordinate of the bearing vector:

    (car (rover-bearing r))

Here's the full code for just the _M_ function:

    (define (M r)
      (let ((bx (car (rover-bearing r)))
            (by (cdr (rover-bearing r))))
        (rover (+ bx (rover-x r)) (+ by (rover-y r)) (rover-bearing r))))

What I really wanted was to extract these components from the _rover_ structure using pattern matching, as is common in more pure functional languages, but this dialect of Scheme doesn't come with support for that out of the box. Yes, there are third-party libraries that support it, but I wanted to limit myself to just the core features of the language.
