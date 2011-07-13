using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using MarsRovers;

namespace Test
{
    class Test: UsingRoverCommands
    {
        static void Main(string[] args)
        {
            Rover r1 = new Rover(1, 2, UnitVector.N);
            r1.Execute(L,M,L,M,L,M,L,M,M);
            Assert("Test 1", 1 == r1.Position.x && 3 == r1.Position.y
                    && UnitVector.N == r1.Heading);

            Rover r2 = new Rover(3, 3, UnitVector.E);
            r2.Execute(M,M,R,M,M,R,M,R,R,M);
            Assert("Test 2", 5 == r2.Position.x && 1 == r2.Position.y
                    && UnitVector.E == r2.Heading);

            Console.WriteLine("Tests passed!");
            Console.ReadLine();
        }

        private static void Assert(string msg, bool assertion)
        {
            if (!assertion)
            {
                Console.WriteLine("Assertion failed: " + msg);
                Console.ReadLine();
                System.Environment.Exit(1);
            }
        }
    }
}
