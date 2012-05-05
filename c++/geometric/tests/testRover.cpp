// Simple assert-based tests (avoid dependency on NUnit or other third-party tool).


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


void testRover()
{
	testConstructRover();
}