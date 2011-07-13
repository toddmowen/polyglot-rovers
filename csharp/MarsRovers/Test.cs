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
            Rover r1 = new Rover(1, 2, Vector.N);
            r1.Execute(L,M,L,M,L,M,L,M,M);
            Assert("Test 1", new Rover(1, 3, Vector.N).Equals(r1));

            Rover r2 = new Rover(3, 3, Vector.E);
            r2.Execute(M,M,R,M,M,R,M,R,R,M);
            Assert("Test 2", new Rover(5, 1, Vector.E).Equals(r2));

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
