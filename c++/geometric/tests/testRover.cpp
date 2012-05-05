// Simple assert-based tests (avoid dependency on NUnit or other third-party tool).


#include <algorithm>
#include <assert.h>
#include <rovers.h>


void testConstructRover()
{
	rovers::Vec2 velocity(4,3);
	rovers::Rover rover(10, -5, velocity);

	assert(10 == rover.x());
	assert(-5 == rover.y());
	assert(velocity == rover.velocity());
}


void testM()
{
	rovers::Rover rover(10, -5, rovers::Vec2(1,2));
	rover.M();

	assert(10+1 == rover.x());
	assert(-5+2 == rover.y());
}


void testL()
{
	rovers::Rover rover(10, -5, rovers::Vec2(1,0));
	rover.L();

	assert(rovers::Vec2(0,1) == rover.velocity());
}


void testR()
{
	rovers::Rover rover(10, -5, rovers::Vec2(1,0));
	rover.R();

	assert(rovers::Vec2(0,-1) == rover.velocity());
}


void testExec()
{
	// sample input from the project description:
	rovers::Rover rover(1, 2, rovers::Vec2(0,1));
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
	assert(rovers::Vec2(0,1) == rover.velocity());
}


void testRover()
{
	testConstructRover();
	testM();
	testL();
	testR();
	testExec();
}