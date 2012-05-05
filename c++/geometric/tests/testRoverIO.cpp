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


void testRoverIO()
{
	testRoverWrite();
}