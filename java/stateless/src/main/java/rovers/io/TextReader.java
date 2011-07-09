package rovers.io;

import java.io.IOException;
import java.io.LineNumberReader;
import java.io.Reader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import static rovers.Vectors.*;

import rovers.Bearings;
import rovers.Commands;
import rovers.Mission;
import rovers.Program;
import rovers.State;
import rovers.Transformation;

/**
 * Read the application's input objects from a character stream. Provides both methods
 * for reading individual objects, and also a readMission() method for reading the
 * complete input to the application.
 * 
 * This class extends LineNumberReader, so that getLineNumber() is available to the
 * caller for use in reporting errors, etc. Internally the line numbers are not
 * used; only the caller is in a position to know whether line numbers would be
 * meaningful for the particular stream being read.
 */
public class TextReader extends LineNumberReader {

	// Recognized compass bearings: a mapping from *uppercase* letters
	private static Map<String,int[]> BEARINGS = new HashMap<String,int[]>();
	{{
		BEARINGS.put("N", Bearings.N);
		BEARINGS.put("E", Bearings.E);
		BEARINGS.put("S", Bearings.S);
		BEARINGS.put("W", Bearings.W);
	}}
	
	// Recognized commands: a mapping from *uppercase* letters
	private static Map<String,Transformation> COMMANDS = new HashMap<String,Transformation>();
	{{
		COMMANDS.put("L", Commands.L);
		COMMANDS.put("R", Commands.R);
		COMMANDS.put("M", Commands.M);
	}}
	
	public TextReader(Reader in) {
		super(in);
	}

	/**
	 * Interpret the next input line as a 2d vector and return it.
	 */
	public int[] readVector() throws InvalidFormat, IOException {
		String line = readLine();
		if (line == null) return null;
		
		// Match a line containing two non-negative integers separated by whitespace.
		Matcher matcher = Pattern.compile("^\\s*(\\d+)\\s+(\\d+)\\s*$").matcher(line);
		if (!matcher.find()) throw new InvalidFormat("Expecting an integer vector.");
		
		return vector(Integer.valueOf(matcher.group(1)), Integer.valueOf(matcher.group(2)));
	}
	
	/**
	 * Interpret the next input line as a State and return it;
	 */
	public State readState() throws InvalidFormat, IOException {
		String line = readLine();
		if (line == null) return null;
		
		// Match a line containing two integers and a symbol, all separated by whitespace.
		Matcher matcher = Pattern.compile("^\\s*(\\d+)\\s+(\\d+)\\s*(\\S+)\\s*$").matcher(line);
		if (!matcher.find()) throw new InvalidFormat("Expecting a state.");

		String bearingSymbol = matcher.group(3);
		int[] velocity = BEARINGS.get(bearingSymbol.toUpperCase());
		if (velocity == null) throw new InvalidFormat(String.format("Unrecognized bearing: '%s'", bearingSymbol));

		int[] position = vector(Integer.valueOf(matcher.group(1)), Integer.valueOf(matcher.group(2)));
		return new State(position, velocity);
	}
	
	/**
	 * Interpret the next input line as a Program and return it.
	 */
	public Program readProgram() throws InvalidFormat, IOException {
		String line = readLine();
		if (line == null) return null;
		
		List<Transformation> ts = new ArrayList<Transformation>();
		
		// Split into single-letter symbols, optionally separated by whitespace
		Matcher matcher = Pattern.compile("\\s*(\\S)\\s*").matcher(line);
		while (matcher.find()) {
			String commandSymbol = matcher.group(1);
			Transformation t = COMMANDS.get(commandSymbol.toUpperCase());
			if (t == null) throw new InvalidFormat(String.format("Unrecognized command: '%s'", commandSymbol));
			ts.add(t);
		}

		return new Program(ts.toArray(new Transformation[ts.size()]));
	}

	/**
	 * Read to the end of the input stream, interpretting it as an input scenario,
	 * and return the results as a Mission object.
	 */
	public Mission readMission() throws InvalidFormat, IOException {
		int[] bounds = readVector();
		if (bounds == null) throw new InvalidFormat("Input is empty.");
		
		List<Mission.RoverConfig> rovers = new ArrayList<Mission.RoverConfig>();
		State initialState;
		while((initialState = readState()) != null) {
			Program program = readProgram();
			if (program == null) throw new InvalidFormat("Rover command sequence missing.");
			
			rovers.add(new Mission.RoverConfig(initialState, program));
		}
		
		return new Mission(bounds, rovers);
	}
	
}
