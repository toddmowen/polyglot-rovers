package rovers;

/**
 * Rover commands are represented as objects that implement this interface.
 * The application may call the apply() method more than once, and implementations
 * must make the following guarantees:
 * 
 * - The only side-effect of the method call is to modify the contents of
 *   the two vectors passed it it as arguments.
 * - For any given values of position and velocity, the effect of the method
 *   should always be the same no matter how many times it is called.
 */
public interface Transformation {

	public void apply(int[] position, int[] velocity);
	
}
