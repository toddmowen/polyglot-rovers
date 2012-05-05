// Simple assert-based tests (avoid dependency on NUnit or other third-party tool).


#include <sstream>
#include <assert.h>
#include <rovers.h>

using rovers::Rover;


void testRoverWrite()
{
	std::stringstream buf;
	Rover rover(4, 1, Rover::EAST);
	buf << rover;

	assert("4 1 E" == buf.str());
}


void testRoverRead()
{
	std::stringstream buf("3 2 S");
	Rover rover(0, 0, Rover::NORTH);
	buf >> rover;

	assert(Rover(3, 2, Rover::SOUTH) == rover);
}


void testRoverIO()
{
	testRoverWrite();
	testRoverRead();
}