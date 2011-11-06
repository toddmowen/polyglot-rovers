package rovers

case class Vector(x: Int, y: Int) {
	def +(v: Vector) = Vector(x + v.x, y + v.y)
	def quarterTurn = Vector(y, -x)

	// infinite stream: this, this+90deg, this+180deg, ...
	def andQuarterTurns: Stream[Vector] = this #:: this.quarterTurn.andQuarterTurns
}

case class Rover(pos: Vector, dir: Vector) {
	override def toString = pos.x + " " + pos.y + " " + Rover.directionToName(dir)
}

object Rover {
	val directions = Seq("N", "E", "S", "W").zip(Vector(0, 1).andQuarterTurns) toMap
	val directionToName = directions map (_.swap)

	val commands = Map(
		"R" -> ((r: Rover) => Rover(r.pos, r.dir.quarterTurn)),
		"L" -> ((r: Rover) => Rover(r.pos, r.dir.quarterTurn.quarterTurn.quarterTurn)),
		"M" -> ((r: Rover) => Rover(r.pos + r.dir, r.dir))
	)
}
