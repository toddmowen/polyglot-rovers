package rovers;

import org.junit.Test;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static rovers.Bearings.*;
import static rovers.Commands.*;
import static rovers.Vectors.*;


/**
 * Verify that commands and bearings interact as expected. 
 */
public class CommandTest {

	private int[] p = vector(2,5);
	private int[] v = copyVector(N);

	private void assertPosition(int x, int y, int[] velocity) {
		assertEquals(x, p[0]);
		assertEquals(y, p[1]);
		assertTrue(vectorEquals(velocity, v));
	}
	
	@Test
	public void testRotation() {
		L.apply(p, v); assertPosition(2, 5, W);
		L.apply(p, v); assertPosition(2, 5, S);
		L.apply(p, v); assertPosition(2, 5, E);
		L.apply(p, v); assertPosition(2, 5, N);

		R.apply(p, v); assertPosition(2, 5, E);
		R.apply(p, v); assertPosition(2, 5, S);
		R.apply(p, v); assertPosition(2, 5, W);
		R.apply(p, v); assertPosition(2, 5, N);
	}
	
	@Test
	public void testMovement() {
		M.apply(p, v); assertPosition(2, 6, N);
		M.apply(p, v); assertPosition(2, 7, N);
		
		v = copyVector(W);
		M.apply(p, v); assertPosition(1, 7, W);
		M.apply(p, v); assertPosition(0, 7, W);
		
		// Transformation.apply does *not* check the plateau bounds
		M.apply(p, v); assertPosition(-1, 7, W);
		
		v = copyVector(E);
		M.apply(p, v); assertPosition(0, 7, E);
		
		v = copyVector(S);
		M.apply(p, v); assertPosition(0, 6, S);
		M.apply(p, v); assertPosition(0, 5, S);
		M.apply(p, v); assertPosition(0, 4, S);
	}
	
}
