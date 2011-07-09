package rovers;

import java.util.Arrays;
import java.util.List;
import org.jmock.Expectations;
import org.jmock.Mockery;
import org.jmock.lib.legacy.ClassImposteriser;
import org.junit.Test;
import static org.junit.Assert.assertEquals;

import rovers.Mission.RoverConfig;
import static rovers.Bearings.*;
import static rovers.Vectors.*;

public class MissionTest {

	private final Mockery mockery = new Mockery() {{ setImposteriser(ClassImposteriser.INSTANCE); }};
	private final Program mockProg1 = mockery.mock(Program.class, "mockProg1");
	private final Program mockProg2 = mockery.mock(Program.class, "mockProg2");

	// All inputs and outputs are just arbitrary values
	private final int[] bounds = vector(3,2);
	private final State input1 = new State(1,2,S);
	private final State output1 = new State(2,3,N);
	private final State input2 = new State(0,0,E);
	private final State output2 = new State(1,1,W);	

	@Test
	public void testEndStates() throws Exception {
		mockery.checking(new Expectations() {{
			allowing(mockProg1).endState(with(input1), with(bounds)); will(returnValue(output1));
			allowing(mockProg2).endState(with(input2), with(bounds)); will(returnValue(output2));
		}});

		List<RoverConfig> rovers = Arrays.asList(new RoverConfig(input1, mockProg1), new RoverConfig(input2, mockProg2));
		Mission mission = new Mission(bounds, rovers);
		List<State> endStates = mission.endStates();

		assertEquals(2, endStates.size());
		assertEquals(output1, endStates.get(0));		
		assertEquals(output2, endStates.get(1));
		mockery.assertIsSatisfied();
	}
	
}
