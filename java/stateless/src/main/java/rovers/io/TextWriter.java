package rovers.io;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.Writer;
import java.util.HashMap;
import java.util.Map;

import rovers.Bearings;
import rovers.State;

public class TextWriter extends BufferedWriter {

	// Mapping from velocity vectors to compass bearings.
	// Since vectors (represented as integer arrays) are not suitable choices for a key
	// (their hashCode and equals methods are the defaults inherited from Object), use a
	// string representation of the vector instead.
	private static Map<String,String> BEARINGS = new HashMap<String,String>();
	{{
		BEARINGS.put(vectorKey(Bearings.N), "N");
		BEARINGS.put(vectorKey(Bearings.E), "E");
		BEARINGS.put(vectorKey(Bearings.S), "S");
		BEARINGS.put(vectorKey(Bearings.W), "W");
	}}
		
	private static String vectorKey(int[] v) {
		return String.format("%d %d", v[0], v[1]);
	}
	
	public TextWriter(Writer out) {
		super(out);
	}

	public void writeState(State state) throws IOException {
		String bearingSymbol = BEARINGS.get(vectorKey(state.velocity()));
		write(String.format("%d %d %s", state.x(), state.y(), bearingSymbol));
		newLine();
	}	
	
}
