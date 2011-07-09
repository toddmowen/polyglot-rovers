Mars Rovers - Java Solution
===========================


COMPILATION
-----------

To compile and run unit tests (requires Maven 2), type:

    mvn package

Or, on unix systems, the integration test (a bash script) can be included in
the build process too by typing: 

    mvn verify
    
In both cases the output package is target/rovers.jar. 

For compilation without third-party build tools, the simplest command would be:

    cd src/main/java ; javac rovers/App.java



EXECUTION
---------

Type:

    java -cp target/rovers.jar rovers.App INPUTFILE

Will also read from standard input if the argument is "-".



EXAMPLES
--------

Input:

    5 5
    1 2 N
    LMLMLMLMM
    3 3 E
    MMRMMRMRRM

Output:

    1 3 N
    5 1 E    


Input:

    4 2
    4 2 S
    MMRMMMMLLLM
    3 0 S
    
    0 2 S
    MMLMLLM

Output:

    0 1 N
    3 0 S
    0 0 W


Input:
    
    4 2
    3 0 S
    RMM
    0 2 S
    MMMLMLLM

Error:

    Rover 2: Fell off the plateau at step 3.


Input:

    5 5
    1 2 N
    LMLMLNLMM

Error:

    Parse error at line 3: Unrecognized command: 'N'



ASSUMPTIONS
-----------

The program should read from a file named on the command line, and write to
standard output.

All coordinates are non-negative (this is implied by the problem description,
assuming that "lower-left" is synonymous with "south-west").

The program should fail with an error if any rover goes beyond the bounds of
the plateau (and output no results, even if other rovers have valid input).

Rovers will never collide with each other (to navigate the surface of an alien
planet, they are presumably sufficiently advanced to avoid obstacles in their
direct path).

Additional whitespace between tokens in the input is ignored.

Blank lines in the input are *not* ignored. For example, an empty line is a
valid (zero-length) command sequence to a rover, while on the other hand
trailing newlines will cause a parse error.



DESIGN PRINCIPLES
-----------------

For ease of calculation, rover positions and headings are represented as two
dimensional vectors with integer coordinates. In Java, their type is int[].
Internally the code always assumes that such values are non-null and have a
length of 2, unless the value might be coming from outside the package (such
as in the State or Mission constructor).

At run time, conversion between vector representation and compass-point
representation is encapsulated in the TextReader and TextWriter classes. The
symbols "N", "S", "E" and "W" are also available in the source code, however,
as names for the corresponding unit vectors, and are used extensively in unit
tests in order to match the notation used in the problem description. The
constructors for State and Program are similarly inspired by the syntax of the
input file, e.g. new State(1,2,N) or new Program(L,M,M,R). This could be
considered a very limited example of an embedded DSL.

The core types, State, Program and Mission, are all immutable (see DESIGN
RATIONALE, below). Because Java's standard library does not provide immutable
array or list classes, we instead rely upon "defensive" copying in the
constructors and getters to guarantee immutability. (Using immutable types from
any one of several third-party collections libraries would have been another
alternative). The immutable classes have not been declared "final", simply
because this makes testing with jMock more difficult.

I/O concerns have been isolated from the core logic, in fact they have even
been placed in a separate sub-package. This includes all the details about the
input and output formats. Changing the format, or even adding a new format such
as an XMLReader, could be easily achieved without changing other parts of the
code.

Input parsing is based on regular expressions, but for bearings and commands
the initial match is not on a character group such as "[NSEW]", but on any
non-whitespace symbol, which is then looked up in a hash map. In other words,
the actual input grammar is a subset of what the regular expression allows.
This compromise has the advantages of (a) reducing the complexity of the regex,
(b) making it easy to extend the application (by adding new commands, for
example) and, most importantly in my opinion, (c) making more helpful error
messages possible!



DESIGN RATIONALE
----------------

One way to approach object-oriented design is to model real-world actors as
objects. That approach, which would lead to writing classes such as Rover and
Plateau, is not the one that I have taken here. The ability to track the
current state of multiple Rover objects would be handy in a real-time control
system, but the problem at hand is more of an analytical one: we only wish to
know where a rover WILL BE after executing its commands, not where it is RIGHT
NOW. Moveover, we are only interested in the results of the simulation, and
not its side-effects.

Given this observation, I have sought to avoid a stateful interface. Instead,
immutable objects are used to represent the inputs to the program, and their
public methods offer a relatively high-level interface for asking questions
such as "where will this rover be after executing this command sequence"?

This state-free principle does *not* extend to the implementation details,
however. Notably, the rover commands are represented using the Transformation
interface consisting of an apply() method which is called *only* for its side
effects. This decision is a pragmatic one, being simpler to express and better
performing in Java than would be possible with a more "functional" style.



DESIGN REGRETS
--------------

My original decision to use the int[] type for vectors was mainly to ensure
terse code. While this was indeed convenient for coding, it also had some
disadvantages that could have been avoided by using an immutable wrapper class
for vectors:

1. One problem is that the equals() and hashCode() methods of int[] are the
   naive implementations inherited from Object, making these values
   unsuitable for use as keys in the Map implementations provided by the
   standard library. In TextWriter, for example, I was forced to convert
   vectors to strings in order to use them in a HashMap.

2. A more worrying consequence is that because such values are mutable it is
   necessary to explicitly call Vectors.copyVector() whenever immutability
   is required. And no guarantee at all can be made for the static final
   members of the Bearings class: N, S, E, W. Though these are nominally
   "constants", it is actually possible for their contents to be accidentally
   changed at runtime!
