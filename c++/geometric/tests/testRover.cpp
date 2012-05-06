// Simple assert-based tests (avoid dependency on NUnit or other third-party tool).


#include <algorithm>
#include <assert.h>
#include <rovers.h>

using rovers::Rover;


void
testConstructRover()
{
	Rover rover(10, -5, Rover::WEST());

	assert (10 == rover.x());
	assert (-5 == rover.y());
	assert (Rover::WEST() == rover.heading());
}


void
testDefaultConstructRover()
{
	Rover rover;

	// Default constructor is provided for convenience, but it leaves the rover
	// in a meaningless state, in particular it has no legal heading.
	assert (Rover::EAST() != rover.heading());
	assert (Rover::NORTH() != rover.heading());
	assert (Rover::WEST() != rover.heading());
	assert (Rover::SOUTH() != rover.heading());
}


void
testM()
{
	Rover rover(10, -5, Rover::SOUTH());
	rover.M();

	assert (10 == rover.x());
	assert (-6 == rover.y());
}


void
testL()
{
	Rover rover(10, -5, Rover::EAST());
	rover.L();

	assert (Rover::NORTH() == rover.heading());
}


void
testR()
{
	Rover rover(10, -5, Rover::EAST());
	rover.R();

	assert (Rover::SOUTH() == rover.heading());
}


void
testExec()
{
	// sample input from the project description:
	Rover rover(1, 2, Rover::NORTH());
	Rover::Command cmds[] = {
		&Rover::L,
		&Rover::M,
		&Rover::L,
		&Rover::M,
		&Rover::L,
		&Rover::M,
		&Rover::L,
		&Rover::M,
		&Rover::M
	};

	std::for_each(
		std::begin(cmds),
		std::end(cmds),
		[&] (Rover::Command cmd) { rover.exec(cmd); } 
	);

	assert (1 == rover.x());
	assert (3 == rover.y());
	assert (Rover::NORTH() == rover.heading());
}


void
testRover()
{
	testConstructRover();
	testDefaultConstructRover();
	testM();
	testL();
	testR();
	testExec();
}