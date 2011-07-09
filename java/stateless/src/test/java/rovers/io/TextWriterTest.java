package rovers.io;

import java.io.BufferedReader;
import java.io.PipedReader;
import java.io.PipedWriter;
import org.junit.Test;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import rovers.State;
import static rovers.Bearings.*;


public class TextWriterTest {
	
	@Test
	public void testWriteState() throws Exception {
		PipedWriter out = new PipedWriter();
		TextWriter textWriter = new TextWriter(out);

		BufferedReader pipe = new BufferedReader(new PipedReader(out));

		textWriter.writeState(new State(22,4,W));
		textWriter.writeState(new State(8,14,N));
		textWriter.writeState(new State(0,111,E));
		textWriter.writeState(new State(5,5,S));
		textWriter.close();  // ensure content is flushed
		
		assertEquals("22 4 W", pipe.readLine());
		assertEquals("8 14 N", pipe.readLine());
		assertEquals("0 111 E", pipe.readLine());
		assertEquals("5 5 S", pipe.readLine());
		assertNull(pipe.readLine());
	}
	
}
