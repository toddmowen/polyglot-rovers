using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace MarsRovers
{
    public class Vector
    {
        // Vector instances are immutable
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
        // instances are the only ones that will ever exist:
        public static readonly UnitVector N = new UnitVector(0, 1);
        public static readonly UnitVector E = new UnitVector(1, 0);
        public static readonly UnitVector S = new UnitVector(0, -1);
        public static readonly UnitVector W = new UnitVector(-1, 0);

        private static readonly UnitVector[] ClockwiseOrder = { N, E, S, W };
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
        public Vector Position { get; private set; }
        public UnitVector Heading { get; private set; }

        public Rover(int x, int y, UnitVector heading)
        {
            this.Position = new Vector(x, y);
            this.Heading = heading;
        }

        public void L() { Heading = Heading.Left; }
        public void R() { Heading = Heading.Right; }
        public void M() { Position = Position + Heading; }

        public delegate void RoverCommand(Rover r);
        public void Execute(params RoverCommand[] commands)
        {
            foreach (RoverCommand cmd in commands) cmd(this);
        }

        // Because the language doesn't have a straightforward way to create
        // an "open instance delegate" (a delegate to an instance method, but
        // not closed over a specific instance), this helper function is
        // provided to make obtaining the RoverCommands simpler.
        public static RoverCommand CommandByName(String name)
        {
            return (RoverCommand) Delegate.CreateDelegate(
                    typeof(RoverCommand), null, typeof(Rover).GetMethod(name));
        }
    }

    // As a further convenience, classes can inherit from UsingRoverCommands in
    // order to access the L, R, and M commands without qualifiers. (Many people
    // would call this an anti-pattern; I won't try to deny it).
    public abstract class UsingRoverCommands
    {
        protected static readonly Rover.RoverCommand L = Rover.CommandByName("L");
        protected static readonly Rover.RoverCommand R = Rover.CommandByName("R");
        protected static readonly Rover.RoverCommand M = Rover.CommandByName("M");
    }
}
