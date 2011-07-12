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
            get { return ClockwiseOrder[(MyPosition() + 1) % 4]; }
        }

        public UnitVector Left
        {
            get { return ClockwiseOrder[(MyPosition() + 3) % 4]; }
        }
    }

    public class Rover
    {
        // Field have internal write accessibility, because they are
        // modified by extension methods defined in RoverCommands.
        public Vector Position { get; internal set; }
        public UnitVector Heading { get; internal set; }

        public Rover(int x, int y, UnitVector heading)
        {
            this.Position = new Vector(x, y);
            this.Heading = heading;
        }

        public delegate void RoverCommand(Rover r);
        public void Execute(params RoverCommand[] commands)
        {
            foreach (RoverCommand cmd in commands)
            {
                cmd(this);
            }
        }
    }

    public static class RoverCommands
    {
        public static void L(this Rover r) { r.Heading = r.Heading.Left; }
        public static void R(this Rover r) { r.Heading = r.Heading.Right; }
        public static void M(this Rover r) { r.Position = r.Position + r.Heading; }
    }

    // Tests can inherit from this class in order to access commands as "L",
    // rather than "RoverCommands.L". Yes, this is a hack / anti-pattern.
    public abstract class UsingRoverCommands
    {
        protected static Rover.RoverCommand L = RoverCommands.L;
        protected static Rover.RoverCommand R = RoverCommands.R;
        protected static Rover.RoverCommand M = RoverCommands.M;
    }
}
