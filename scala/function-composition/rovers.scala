package rovers

object Directions extends Enumeration {
	type Direction = Value
	val N, E, S, W = Value

	def unitVector(d: Value) = d match {
		case N => (0, 1)
		case E => (1, 0)
		case S => (0, -1)
		case W => (-1, 0)
	}

	def right(d: Value) = Directions((d.id + 1) % maxId)
	def left(d: Value) = Directions((d.id + maxId - 1) % maxId)
}
import Directions.{Direction, N, E, S, W, unitVector, right, left}


case class Rover(x: Int, y: Int, d: Direction)


object Commands {
	type Command = (Rover) => Rover

	// Compose a list of Commands into a new Command
	def apply(commands: Command*) = commands.reduceLeft(_ andThen _)

	val L: Command = _ match {
		case Rover(x, y, d) => Rover(x, y, left(d))
	}

	val R: Command = _ match {
		case Rover(x, y, d) => Rover(x, y, right(d))
	}

	val M: Command = _ match {
		case Rover(x, y, d) => unitVector(d) match {
			case (dx, dy) => Rover(x + dx, y + dy, d)
		}
	}
}
