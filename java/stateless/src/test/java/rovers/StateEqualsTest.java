package rovers;

import org.junit.Test;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;

import static rovers.Bearings.*;


/**
 * Test State.equals(), which is not used in the application but is depended on
 * by many unit tests.
 */
public class StateEqualsTest {

	@Test
	public void testEquals() {
		State pos = new State(5,6,S);
		
		assertTrue(pos.equals(pos));
		assertTrue(pos.equals(new State(5,6,S)));

		assertFalse(pos.equals(null));
		assertFalse(pos.equals(new Object()));
		assertFalse(pos.equals(new State(6,5,S)));
		assertFalse(pos.equals(new State(5,5,S)));
		assertFalse(pos.equals(new State(6,6,S)));
		assertFalse(pos.equals(new State(5,6,E)));
		assertFalse(pos.equals(new State(5,6,W)));
		assertFalse(pos.equals(new State(5,6,N)));
	}
	
}
