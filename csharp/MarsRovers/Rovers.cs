using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace MarsRovers
{
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

    public class UnitVector: Vector
    {
        private UnitVector(int x, int y) : base(x, y) { }

        // Since there is no public constructor, the following four
        // instances are the only ones that should ever exist:
        public static UnitVector N = new UnitVector(0, 1);
        public static UnitVector E = new UnitVector(1, 0);
        public static UnitVector S = new UnitVector(0, -1);
        public static UnitVector W = new UnitVector(-1, 0);

        private static UnitVector[] ClockwiseOrder = { N, E, S, W };
        private int MyPosition() { return Array.IndexOf(ClockwiseOrder, this); }

        public UnitVector Right
        {
            get { return ClockwiseOrder[MyPosition() + 1 % 4]; }
        }

        public UnitVector Left
        {
            get { return ClockwiseOrder[MyPosition() + 3 % 4]; }
        }
    }

    public class Rover
    {
        private Vector Position;
        private UnitVector Heading;

        public Rover(int x, int y, UnitVector heading)
        {
            this.Position = new Vector(x, y);
            this.Heading = heading;
        }

        public void L() { Heading = Heading.Left; }
        public void R() { Heading = Heading.Right; }
        public void M() { Position = Position + Heading; }
    }
}
