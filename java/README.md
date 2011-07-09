Java was the first language I used to solve this problem.


First solution: stateless
-------------------------

This solution was an attempt to write a solution that minimizes "state". The problem of statefulness has been discussed quite a lot in recent years, and I have certainly developed a fondness for immutable objects. So in this solution, I rejected the "natural mapping" of the problem to domain objects such as Plateau and Rover, in favour of a model which reduced rover coordinates to mere "values", to be returned by things called Transformations (representing the rover commands).

An additional characteristic of this solution, which is mainly related to the fact that I originally submitted this solution for assessment purposes, is that it tries to be very "complete", with bounds checking, unit tests, a Maven build script, and so on.

Note that this solution implements the "L" and "R" rover commands as geometric transformations. In the Scala solutions that I wrote later, I used a simpler design based on assigning an order to the bearings, i.e.. N -> E -> S -> W.
