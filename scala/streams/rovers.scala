package rovers

case class Vector(x: Int, y: Int) {
	def +(v: Vector) = Vector(x + v.x, y + v.y)
	def quarterTurn = Vector(y, -x)
}

case class Rover(pos: Vector, dir: Vector) {
	override def toString = pos.x + " " + pos.y + " " + Rover.directionToName(dir)

	// implicit conversion allows parameter to be an ordinary String too
	def execute(cmds: Seq[Char]): Rover = cmds match {
		case Nil => this
		case Seq(cmd, rest @ _*) => Rover.commands(cmd)(this) execute rest
	}
}

object Rover {
	val directions = Seq("N", "E", "S", "W")
		.zip(Stream.iterate(Vector(0, 1))(_.quarterTurn))
		.toMap
	val directionToName = directions map (_.swap)

	val commands = Map(
		'R' -> ((r: Rover) => Rover(r.pos, r.dir.quarterTurn)),
		'L' -> ((r: Rover) => Rover(r.pos, r.dir.quarterTurn.quarterTurn.quarterTurn)),
		'M' -> ((r: Rover) => Rover(r.pos + r.dir, r.dir))
	)

	private val roverRegex = """([0-9]+)\s+([0-9]+)\s+(\w)""".r
	def apply(s: String): Rover = {
		val roverRegex(xstr, ystr, dirstr) = s
		Rover(Vector(xstr.toInt, ystr.toInt), directions(dirstr))
	}
}

object Test {
	def main(args: Array[String]) = {
		assert("1 3 N" == (Rover("1 2 N") execute "LMLMLMLMM" toString))
		assert("5 1 E" == (Rover("3 3 E") execute "MMRMMRMRRM" toString))
		println("Tests pass")
	}
}
