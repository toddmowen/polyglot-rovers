package rovers;

import java.util.Arrays;

/**
 * Convenience methods for working with 2d vectors, which are represented in this
 * application simply as integer arrays. The static methods provided by this class
 * are mainly useful for giving meaningful names to these operations.
 */
public class Vectors {

	/**
	 * Create a vector from two integer arguments.
	 */
	public static int[] vector(int x, int y) { return new int[] {x,y}; }
	
	/**
	 * Returns true if v is a non-null, two-dimensional vector.
	 */
	public static boolean isVector(int[] v) { return v != null && v.length == 2; }
	
	/**
	 * Copy v, which is assumed to be a non-null, two-dimensional vector.
	 */
	public static int[] copyVector(int[] v) { return Arrays.copyOf(v, 2); }

	/**
	 * Compare two arguments, which are assumed to be non-null, two-dimensional vectors.
	 */
	public static boolean vectorEquals(int[] v, int[] w) { return Arrays.equals(v, w); }
}
