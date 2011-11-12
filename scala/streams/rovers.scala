package rovers

case class Vector(x: Int, y: Int) {
	def +(v: Vector) = Vector(x + v.x, y + v.y)
	def quarterTurn = Vector(y, -x)
}

case class Rover(pos: Vector, dir: Vector) {
	override def toString = pos.x + " " + pos.y + " " + Rover.directionToName(dir)

	def execute(cmd: Char) = Rover.commands(cmd)(this)
	def execute(cmds: String): Rover = cmds.foldLeft(this)(_ execute _)
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

object Main {
	def main(args: Array[String]) = {
		executeLines(io.Source.stdin.getLines.toStream) foreach println
	}

	def executeLines(lines: Stream[String]): Stream[Rover] = lines match {
		// ignore plateau line
		case plateauLine #:: roverLines => executeRoverLines(roverLines)
	}

	def executeRoverLines(lines: Stream[String]): Stream[Rover] = lines match {
		case Stream.Empty => Stream.Empty
		case rstr #:: cmds #:: rest =>
			(Rover(rstr) execute cmds) #:: executeRoverLines(rest)
	}
}

object Test {
	def main(args: Array[String]) = {
		assert("1 3 N" == (Rover("1 2 N") execute "LMLMLMLMM" toString))
		assert("5 1 E" == (Rover("3 3 E") execute "MMRMMRMRRM" toString))
		assert(Stream("1 3 N", "5 1 E") ==
			Main.executeLines(Stream(
				"5 5",
				"1 2 N",
				"LMLMLMLMM",
				"3 3 E",
				"MMRMMRMRRM"
			)).map(_.toString))
		println("Tests pass")
	}
}
