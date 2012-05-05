#include <iostream>
#include <vector>
#include <algorithm>
#include <rovers.h>

using rovers::Rover;


void skipLine(std::istream& is)
{
	is.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
}


int processStream(std::istream& in, std::ostream& out)
{
	// Ignore plateau bounds (not used in the basic solution to the problem)
	skipLine(in);

	while (! in.eof())
	{
		Rover rover(0, 0, Rover::NORTH);
		std::vector<Rover::Command> cmds;

		in >> rover >> cmds;
		
		std::for_each(
			begin(cmds),
			end(cmds),
			[&] (Rover::Command cmd) { rover.exec(cmd); }
		);

		out << rover << std::endl;
	}

	return 0;
}


int main()
{
	try
	{
		processStream(std::cin, std::cout);
		return 0;
	}
	catch (std::exception e)
	{
		std::cerr << e.what() << std::endl;
		return 1;
	}
}