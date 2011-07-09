package rovers;

import java.util.ArrayList;
import java.util.List;
import static rovers.Vectors.*;

/**
 * An immutable representation of a full set of input to the application, consisting
 * of the plateau bounds plus a list of rover configurations (i.e. the inital state
 * or each rover, and its program).
 */
public class Mission {

	public static final class RoverConfig {
		private final State initialState;
		private final Program program;
		
		public RoverConfig(State initialState, Program program) {
			this.initialState = initialState;
			this.program = program;
		}
		
		public State initialState() { return initialState; }
		public Program program() { return program; }
	}

	private final int[] bounds;
	private final List<RoverConfig> rovers;
	
	public Mission(int[] bounds, List<RoverConfig> rovers) {
		if (!isVector(bounds))
			throw new IllegalArgumentException("Bounds must be a non-null, two-dimensional vector.");
		
		this.bounds = copyVector(bounds);
		this.rovers = new ArrayList<RoverConfig>(rovers);  // copy list
	}
	
	public int[] bounds() {
		return copyVector(bounds);
	}
	
	public List<RoverConfig> rovers() {
		return new ArrayList<RoverConfig>(rovers);  // defensive copy
	}

	/**
	 * Returns a list of the positions where each rover will be after executing its
	 * respective program.
	 */
	public List<State> endStates() throws OutOfBounds {
		List<State> endStates = new ArrayList<State>();
		
		for (int i = 0; i < rovers.size(); i++) {
			RoverConfig rover = rovers.get(i);
			
			try {
				endStates.add(rover.program().endState(rover.initialState(), bounds));
			}
			catch (OutOfBounds e) {
				// Prepend rover number to the error message
				throw new OutOfBounds(String.format("Rover %d: %s", i + 1, e.getMessage()));
			}
		}
		
		return endStates;
	}
}
