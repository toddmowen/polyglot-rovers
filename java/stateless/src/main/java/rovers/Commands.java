package rovers;

public class Commands {

	public static final Transformation M = new Transformation() {
		public void apply(int[] p, int[] v) {
			p[0] += v[0];
			p[1] += v[1];
		}
	};
	
	// The rotation function from which L and R below can be derived is:
	// x' = x*cos(theta) - y*sin(theta)
	// y' = x*sin(theta) + y*cos(theta)
	
	public static final Transformation L = new Transformation() {
		public void apply(int[] p, int[] v) {
			int x = v[0];
			int y = v[1];
			
			v[0] = -y;
			v[1] = x;
		}
	};
	
	public static final Transformation R = new Transformation() {
		public void apply(int[] p, int[] v) {
			int x = v[0];
			int y = v[1];
			
			v[0] = y;
			v[1] = -x;
		}
	};
	
}
