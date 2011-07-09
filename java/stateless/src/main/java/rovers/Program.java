package rovers;

import java.util.Arrays;
import static rovers.Vectors.*;

/**
 * An immutable object containing a list of instructions for a rover to carry out.
 */
public class Program {

	private final Transformation[] transformations;
	
	public Program(Transformation... ts) {
		if (ts == null) throw new NullPointerException("Argument to constructor must be non-null.");
		this.transformations = Arrays.copyOf(ts, ts.length);
	}

	/**
	 * Given a rover's initial state and the bounds of the plateau, return the new state
	 * representing the rover's position after executing this program of commands.
	 * 
	 * @throws OutOfBounds if any step of the program would take the rover beyond the plateau.
	 */
	public State endState(State initialState, int[] bounds) throws OutOfBounds {
		if (!isVector(bounds))
			throw new IllegalArgumentException("Bounds must be a non-null, two-dimensional vector.");
		
		int[] position = initialState.position();
		int[] velocity = initialState.velocity();

		if (!inBounds(position, bounds)) {
			throw new OutOfBounds("Initial position is not on the plateau.");
		}
		
		for (int i = 0; i < transformations.length; i++) {
			transformations[i].apply(position, velocity);
			if (!inBounds(position, bounds)) {
				throw new OutOfBounds(String.format("Fell off the plateau at step %d.", i + 1));
			}
		}
		
		return new State(position, velocity);
	}
	
	private boolean inBounds(int[] position, int[] bounds) {
		return (0 <= position[0] && position[0] <= bounds[0]
		     && 0 <= position[1] && position[1] <= bounds[1]);
	}
	
	@Override
	public boolean equals(Object obj) {
		if (obj == null || !(obj instanceof Program)) return false;
		
		Program that = (Program) obj;
		return Arrays.equals(this.transformations, that.transformations);
	}
}
