import rovers.Directions.{N, S, E, W}
import rovers.Rover

object RoverTest {
	def main(args: Array[String]) {
		val result1 = Rover(1, 2, N).L.M.L.M.L.M.L.M.M
		assert (Rover(1, 3, N) == result1)

		val result2 = Rover(3, 3, E).M.M.R.M.M.R.M.R.R.M
		assert (Rover(5, 1, E) == result2)

		println("Tests pass!")
	}
}


