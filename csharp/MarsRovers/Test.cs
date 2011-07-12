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
            Console.WriteLine(r1.Position.x);
            Console.WriteLine(r1.Position.y);

            Rover r2 = new Rover(3, 3, UnitVector.E);
            r2.Execute(M,M,R,M,M,R,M,R,R,M);
            Console.WriteLine(r2.Position.x);
            Console.WriteLine(r2.Position.y);

            Console.ReadLine();
        }
    }
}
