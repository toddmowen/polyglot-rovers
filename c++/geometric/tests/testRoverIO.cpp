// Simple assert-based tests (avoid dependency on NUnit or other third-party tool).


#include <sstream>
#include <assert.h>
#include <rovers.h>

using rovers::Rover;


void testWriteRover()
{
	std::stringstream buf;
	Rover rover(4, 1, Rover::EAST());
	buf << rover;

	assert("4 1 E" == buf.str());
}


void testReadRover()
{
	std::stringstream buf("3 2 S");
	Rover rover;
	buf >> rover;

	assert(Rover(3, 2, Rover::SOUTH()) == rover);
}


void testReadCommands()
{
	std::stringstream buf("LLMRMM");
	std::vector<Rover::Command> cmds;
	cmds.push_back(&Rover::R);  // this will be overwritten
	buf >> cmds;

	std::vector<Rover::Command> expected;
	expected.push_back(&Rover::L);
	expected.push_back(&Rover::L);
	expected.push_back(&Rover::M);
	expected.push_back(&Rover::R);
	expected.push_back(&Rover::M);
	expected.push_back(&Rover::M);
	assert(expected == cmds);
}


void testInvalidCommand()
{
	try
	{
		std::stringstream buf("LMRZ");
		std::vector<Rover::Command> cmds;
		buf >> cmds;
		assert(false);  // previous statement should throw exception
	}
	catch (std::exception e)
	{
		assert(std::string("Not a valid command: Z") == e.what());
	}
}


void testRoverIO()
{
	testWriteRover();
	testReadRover();
	testReadCommands();
	testInvalidCommand();
}