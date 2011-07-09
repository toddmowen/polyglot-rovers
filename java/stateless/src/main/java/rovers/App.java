package rovers;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.util.List;

import rovers.io.InvalidFormat;
import rovers.io.TextReader;
import rovers.io.TextWriter;

public class App {

	/**
	 * An exception to signal to the mainline method that it should terminate immediately.
	 */
	@SuppressWarnings("serial")
	private static class FatalError extends Exception {
		FatalError(String message, Throwable cause) {
			super(message, cause);
		}
	}
	
	public static void main(String[] args) {
		if (args.length != 1) {
			System.err.println("Usage: java rovers.App INPUTFILE");
			System.err.println("   or: java -jar rovers.jar INPUTFILE");
			System.exit(1);
		}

		try {
			Mission mission = readMissionFile(args[0]);
			List<State> endStates = analyzeMission(mission);
			writeStates(System.out, endStates);
		}
		catch (FatalError fe) {
			System.err.println(fe.getMessage());
			System.exit(1);
		}
	}

	private static Mission readMissionFile(String filename) throws FatalError {
		InputStream in = null;
		try {
			if ("-".equals(filename))
				in = System.in;
			else
				in = new FileInputStream(new File(filename));
			
			return readMission(in);
		}
		catch (FileNotFoundException e) {
			throw new FatalError(String.format("File not found: %s", filename), e);
		}
		finally {
			if (in != null) {
				try {
					in.close();
				}
				catch (IOException e) {
					// exception while closing an input stream is safe to ignore
				}
			}
		}
	}
	
	private static Mission readMission(InputStream in) throws FatalError {
		TextReader textReader = new TextReader(new InputStreamReader(in));

		try {
			return textReader.readMission();
		}
		catch (InvalidFormat e) {
			throw new FatalError(String.format("Parse error at line %d: %s",
					textReader.getLineNumber(), e.getMessage()), e);
		}
		catch (IOException e) {
			throw new FatalError(String.format("Error reading input: %s", e.getMessage()), e);
		}
	}
	
	private static List<State> analyzeMission(Mission mission) throws FatalError {
		try {
			return mission.endStates();
		}
		catch (OutOfBounds e) {
			throw new FatalError(e.getMessage(), e);
		}
	}
	
	private static void writeStates(OutputStream out, List<State> states) throws FatalError {
		TextWriter textWriter = new TextWriter(new OutputStreamWriter(out));
		
		try {
			for (State state : states) {
				textWriter.writeState(state);
			}
			textWriter.close();
		}
		catch (IOException e) {
			throw new FatalError(String.format("Error writing output: %s", e.getMessage()), e);
		}
	}
}
