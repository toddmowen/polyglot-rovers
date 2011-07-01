package rovers

object Directions {
	class Direction(val dx: Int, val dy: Int) {
		// Rotation is modelled as modulo arithmetic over the
		// the domain defined by the "ordered" array.
		def +(i: Int) = {
			val positive = i % ordered.length + ordered.length
			val thisIndex = ordered.indexOf(this)
			ordered((thisIndex + positive) % ordered.length)
		}
		def -(i: Int) = this + (-i)
	}

	// Defining constants as case objects theoretically enables pattern
	// matching on them, although we don't use that in this solution.
	// Additionally, a toString method is automatically generated.
	case object N extends Direction(0, 1)
	case object E extends Direction(1, 0)
	case object S extends Direction(0, -1)
	case object W extends Direction(-1, 0)

	val ordered = Vector[Direction](N, E, S, W)
}
import Directions.{Direction, N, E, S, W}


case class Rover(x: Int, y: Int, d: Direction) {
	def L = new Rover(x, y, d - 1)
	def R = new Rover(x, y, d + 1)
	def M = new Rover(x + d.dx, y + d.dy, d)
}
