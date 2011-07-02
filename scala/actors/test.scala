import rovers.Directions.{N, S, E, W}
import rovers.{Rover, RoverActor, L, R, M, GetRover, Exit}

object RoverTest {
	def main(args: Array[String]) {
		val testActor1 = new RoverActor(Rover(1, 2, N))
		List(L, M, L, M, L, M, L, M, M).foreach(testActor1 ! _)
		assert (Rover(1, 3, N) == (testActor1 !? GetRover))
		testActor1 ! Exit

		val testActor2 = new RoverActor(Rover(3, 3, E))
		List(M, M, R, M, M, R, M, R, R, M).foreach(testActor2 ! _)
		assert (Rover(5, 1, E) == (testActor2 !? GetRover))
		testActor2 ! Exit

		println("Tests pass!")
	}
}


