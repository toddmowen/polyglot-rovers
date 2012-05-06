C++ solution to the "Mars Rovers" problem described here: https://github.com/toddmowen/polyglot-rovers#readme

**WARNING:** This is a fairly _unorthodox_ example of C++ code (for example in the lengths it goes to place `N`, `E`, `S` and `W` at the top level of the namespace -- which is also where the name "externs" comes from).

The best place to start reading is probably the public interface set out in `rovers.h`, keeping in mind that most of its oddities are solely aimed at allowing clients to write "domain" level code as tersely as this:

    Rover rover(1, 2, N);
    command_t cmds[] = {L, M, L, M, L, M, L, M, M};
    rover.exec(begin(cmds), end(cmds));
