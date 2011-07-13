using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace MarsRovers
{
    // Immutable value type
    public struct Vector
    {
        public readonly int x;
        public readonly int y;

        public Vector(int x, int y) { this.x = x; this.y = y; }

        public static Vector operator + (Vector a, Vector b)
        {
            return new Vector(a.x + b.x, a.y + b.y);
        }
 
        public static readonly Vector N = new Vector(0, 1);
        public static readonly Vector E = new Vector(1, 0);
        public static readonly Vector S = new Vector(0, -1);
        public static readonly Vector W = new Vector(-1, 0);
        public static readonly Vector[] UnitVectors = { N, E, S, W };

        private int MyPosition()
        {
            int pos = Array.IndexOf(UnitVectors, this);
            if (pos == -1) throw new ArgumentException("Not a unit vector");
            return pos;
        }

        public Vector Right
        {
            get { return UnitVectors[(MyPosition() + 1) % 4]; }
        }

        public Vector Left
        {
            get { return UnitVectors[(MyPosition() + 3) % 4]; }
        }
    }

    // Mutable value type (typically not a good design choice; in this case
    // we only do it out of laziness -- implementing this as a struct means
    // we get structural equality for free, without needing to override the
    // Equals method).
    public struct Rover
    {
        public Vector Position { get; private set; }
        public Vector Heading { get; private set; }

        public Rover(int x, int y, Vector heading) : this()
        {
            Position = new Vector(x, y);
            Heading = heading;
        }

        public void L() { Heading = Heading.Left; }
        public void R() { Heading = Heading.Right; }
        public void M() { Position = Position + Heading; }

        public delegate void RoverCommand(ref Rover r);
        public void Execute(params RoverCommand[] commands)
        {
            foreach (RoverCommand cmd in commands) cmd(ref this);
        }

        // Because the language doesn't have a straightforward way to create
        // an "open instance delegate" (a delegate to an instance method, but
        // not closed over a specific instance), this helper function is
        // provided to make obtaining the RoverCommands simpler.
        public static RoverCommand CommandByName(String name)
        {
            return (RoverCommand)Delegate.CreateDelegate(
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