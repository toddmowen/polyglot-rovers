C++ solution to the "Mars Rovers" problem described here: https://github.com/toddmowen/polyglot-rovers#readme

This solution implements the rover commands in terms of vector operations. The internal representation of each rover's state uses a position and a velocity vector.

I/O is accomplished by overriding the extraction and insertion operators (`>>` and `<<`) on streams.

See `include\rovers.h` for the public interface. For examples of use, see `main.cpp` or unit tests.
