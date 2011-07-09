package rovers.io;

import java.io.StringReader;
import java.util.List;
import org.junit.Test;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import rovers.Mission;
import rovers.Program;
import rovers.State;
import static rovers.Bearings.*;
import static rovers.Commands.*;
import static rovers.Vectors.vector;
import static rovers.Vectors.vectorEquals;


public class TextReaderTest {
	
	@Test
	public void testReadVector() throws Exception {
		StringReader in = new StringReader("7 22");
		TextReader textReader = new TextReader(in);
		
		int[] bounds = textReader.readVector();
		assertTrue(vectorEquals(vector(7,22), bounds));
		
		assertNull(textReader.readVector());  // end of stream
	}
	
	@Test
	public void testReadState() throws Exception {
		StringReader in = new StringReader("41 2 E\n0 332 N");
		TextReader textReader = new TextReader(in);
		
		State state1 = textReader.readState();
		assertEquals(new State(41,2,E), state1);
		
		State state2 = textReader.readState();
		assertEquals(new State(0,332,N), state2);
		
		assertNull(textReader.readState());  // end of stream
	}
	
	@Test
	public void testReadProgram() throws Exception {
		StringReader in = new StringReader("LMLMLMLMM\nMRRMLL");
		TextReader textReader = new TextReader(in);
		
		Program prog1 = textReader.readProgram();
		assertEquals(new Program(L,M,L,M,L,M,L,M,M), prog1);
		
		Program prog2 = textReader.readProgram();
		assertEquals(new Program(M,R,R,M,L,L), prog2);
		
		assertNull(textReader.readProgram());  // end of stream
	}

	@Test
	public void testReadMission() throws Exception {
		StringReader in = new StringReader(
				"3 5\n" +
				"3 2 W\n" +
				"RMMRRMRML\n" +
				"0 0 N\n" +
				"MMRMLMLM\n");
		
		TextReader textReader = new TextReader(in);
		Mission mission = textReader.readMission();
		
		assertTrue(vectorEquals(vector(3,5), mission.bounds()));
		assertEquals(2, mission.rovers().size());
		List<Mission.RoverConfig> rovers = mission.rovers();
		assertEquals(new State(3,2,W), rovers.get(0).initialState());
		assertEquals(new Program(R,M,M,R,R,M,R,M,L), rovers.get(0).program());
		assertEquals(new State(0,0,N), rovers.get(1).initialState());
		assertEquals(new Program(M,M,R,M,L,M,L,M), rovers.get(1).program());
	}
	
}
