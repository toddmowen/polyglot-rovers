package rovers

import scala.actors.Actor


object Directions {
	class Direction(_left: => Direction, _right: => Direction,
			val dx: Int, val dy: Int) {
		def left = _left
		def right = _right
	}

	// This kind of "manual wiring" is error-prone and repetitive, but
	// it's fascinating that such circular declarations are possible.
	case object N extends Direction(W, E, 0, 1)
	case object E extends Direction(N, S, 1, 0)
	case object S extends Direction(E, W, 0, -1)
	case object W extends Direction(S, N, -1, 0)
}
import Directions.{Direction, N, E, S, W}


case class Rover(x: Int, y: Int, d: Direction)


// Messages recognized by RoverActor
case object L
case object R
case object M
case object GetRover
case object Exit

class RoverActor(private var r: Rover) extends Actor {
	def act = loop {
		react {
			case L => r = Rover(r.x, r.y, r.d.left)
			case R => r = Rover(r.x, r.y, r.d.right)
			case M => r = Rover(r.x + r.d.dx, r.y + r.d.dy, r.d)
			case GetRover => reply(r)
			case Exit => exit
		}
	}

	this.start()
}