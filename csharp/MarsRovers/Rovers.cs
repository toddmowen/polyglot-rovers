using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace MarsRovers
{
    public enum Heading { N, E, S, W }

    // extension methods for Headings
    public static class HeadingMethods
    {
        public static Heading Right(this Heading h)
        {
            return (Heading) (((int) h + 1) % 4);
        }

        public static Heading Left(this Heading h)
        {
            return (Heading) (((int) h + 3) % 4);
        }

        public static Vector UnitVector(this Heading h)
        {
            switch (h)
            {
                case Heading.N: return new Vector(0, 1);
                case Heading.E: return new Vector(1, 0);
                case Heading.S: return new Vector(0, -1);
                default: return new Vector(-1, 0);
            }
        }
    }

    public class Vector
    {
        // instances are immutable
        public readonly int x;
        public readonly int y;

        public Vector(int x, int y) { this.x = x; this.y = y; }

        public static Vector operator + (Vector a, Vector b)
        {
            return new Vector(a.x + b.x, a.y + b.y);
        }
    }
}
