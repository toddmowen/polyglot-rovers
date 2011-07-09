package rovers;

import static rovers.Vectors.*;

/**
 * An immutable object representing the state of a rover, consisting of its position
 * and its heading (the latter expressed a a two-dimensional unit vector).
 */
public class State {

	private final int[] position;
	private final int[] velocity;
	
	public State(int[] position, int[] velocity) {
		if (!isVector(position))
			throw new IllegalArgumentException("Position must be a non-null, two-dimensional vector.");
		if (!isVector(velocity))
			throw new IllegalArgumentException("Velocity must be a non-null, two-dimensional vector.");
		
		this.position = copyVector(position);
		this.velocity = copyVector(velocity);
	}
	
	// Alternative constructor that more closely matches the notation used in the problem description.
	public State(int x, int y, int[] velocity) { this(vector(x,y), velocity); }

	public int[] position() { return copyVector(position); }
	
	public int[] velocity() { return copyVector(velocity); }
	
	public int x() { return position[0]; }
	
	public int y() { return position[1]; }
	
	@Override
	public boolean equals(Object obj) {
		if (obj == null || ! (obj instanceof State)) return false;
		
		State that = (State) obj;
		return (vectorEquals(this.position, that.position) && vectorEquals(this.velocity, that.velocity));
	}
}
