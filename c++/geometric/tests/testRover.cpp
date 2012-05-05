// Simple assert-based tests (avoid dependency on NUnit or other third-party tool).


#include <algorithm>
#include <assert.h>
#include <rovers.h>


void testConstructRover()
{
	rovers::Rover rover(10, -5, rovers::Rover::WEST);

	assert(10 == rover.x());
	assert(-5 == rover.y());
	assert(rovers::Rover::WEST == rover.velocity());
}


void testM()
{
	rovers::Rover rover(10, -5, rovers::Rover::SOUTH);
	rover.M();

	assert(10 == rover.x());
	assert(-6 == rover.y());
}


void testL()
{
	rovers::Rover rover(10, -5, rovers::Rover::EAST);
	rover.L();

	assert(rovers::Rover::NORTH == rover.velocity());
}


void testR()
{
	rovers::Rover rover(10, -5, rovers::Rover::EAST);
	rover.R();

	assert(rovers::Rover::SOUTH == rover.velocity());
}


void testExec()
{
	// sample input from the project description:
	rovers::Rover rover(1, 2, rovers::Rover::NORTH);
	rovers::Rover::Command cmds[] = {
		&rovers::Rover::L,
		&rovers::Rover::M,
		&rovers::Rover::L,
		&rovers::Rover::M,
		&rovers::Rover::L,
		&rovers::Rover::M,
		&rovers::Rover::L,
		&rovers::Rover::M,
		&rovers::Rover::M
	};

	std::for_each(
		std::begin(cmds),
		std::end(cmds),
		[&] (rovers::Rover::Command cmd) { rover.exec(cmd); } 
	);

	assert(1 == rover.x());
	assert(3 == rover.y());
	assert(rovers::Rover::NORTH == rover.velocity());
}


void testRover()
{
	testConstructRover();
	testM();
	testL();
	testR();
	testExec();
}