import org.junit.Test;
import static org.junit.Assert.assertEquals;

public class RoversTest
{
	@Test public void example1()
	{
		Rover r = new Rover(1, 2, Direction.N);
		r.execute("LMLMLMLMM");
		assertEquals("1 3 N", r.toString());
	}

	@Test public void example2()
	{
		Rover r = new Rover(3, 3, Direction.E);
		r.execute("MMRMMRMRRM");
		assertEquals("5 1 E", r.toString());
	}

	@Test(expected = RuntimeException.class)
	public void unknownCommandThrowsException()
	{
		Rover r = new Rover(3, 3, Direction.E);
		r.execute("MMRVM");
	}
}