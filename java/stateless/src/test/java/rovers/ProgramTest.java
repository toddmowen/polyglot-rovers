package rovers;
import static org.junit.Assert.assertEquals;
import static rovers.Bearings.*;
import static rovers.Commands.*;
import static rovers.Vectors.*;

import org.junit.Test;

public class ProgramTest {

	@Test
	public void testExampleInput() throws OutOfBounds {
		// example input and output from the problem description	
		int[] bounds = vector(5,5);
		State rover1 = new State(1,2,N);
		Program prog1 = new Program(L,M,L,M,L,M,L,M,M);
		State rover2 = new State(3,3,E);
		Program prog2 = new Program(M,M,R,M,M,R,M,R,R,M);
		
		assertEquals(new State(1,3,N), prog1.endState(rover1, bounds));
		assertEquals(new State(5,1,E), prog2.endState(rover2, bounds));
	}
	
	@Test(expected=OutOfBounds.class)
	public void testStepOutOfBounds() throws OutOfBounds {
		int[] bounds = vector(2,2);
		State state = new State(1,1,E);
		Program prog = new Program(M,M,R,R,M,M);  // rover leaves plateau but then returns!		
		prog.endState(state, bounds);  // program should still report out-of-bounds
	}
	
	@Test(expected=OutOfBounds.class)
	public void testStartOutOfBounds() throws OutOfBounds {
		int[] bounds = vector(2,2);
		State state = new State(3,2,W);  // initial state is out of bounds
		Program prog = new Program(M,M);  // but after first command the rover will be in bounds		
		prog.endState(state, bounds);  // program should report out-of-bounds
	}
	
}
