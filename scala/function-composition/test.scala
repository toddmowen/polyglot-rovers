import rovers.Rover
import rovers.Directions._
import rovers.Commands
import rovers.Commands._

object RoverTest {
	def main(args: Array[String]) = {
		val testRover1 = Rover(1, 2, N)
		val testCommands1 = Commands(L, M, L, M, L, M, L, M, M)
		assert (Rover(1, 3, N) == testCommands1(testRover1))

		val testRover2 = Rover(3, 3, E)
		val testCommands2 = Commands(M, M, R, M, M, R, M, R, R, M)
		assert (Rover(5, 1, E) == testCommands2(testRover2))

		println("Tests pass!")
	}
}