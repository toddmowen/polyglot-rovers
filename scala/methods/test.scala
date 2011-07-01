import rovers.Directions.{N, S, E, W}
import rovers.Rover

object RoverTest {
	def main(args: Array[String]) {
		val testRover1 = Rover(1, 2, N)
		assert (Rover(1, 3, N) == testRover1.L.M.L.M.L.M.L.M.M)

		val testRover2 = Rover(3, 3, E)
		assert (Rover(5, 1, E) == testRover2.M.M.R.M.M.R.M.R.R.M)

		println("Tests pass!")
	}
}


