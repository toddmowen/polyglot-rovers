#include <iostream>
#include <vector>
#include <algorithm>
#include <rovers.h>

using rovers::Rover;


void skipLine(std::istream& is)
{
	is.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
}


void processStream(std::istream& in, std::ostream& out)
{
	in.exceptions(std::ios::failbit | std::ios::badbit);

	// Ignore plateau bounds (not used in the basic solution to the problem)
	skipLine(in);

	while (! in.eof())
	{
		Rover rover;
		std::vector<Rover::Command> cmds;

		in >> rover >> cmds;
		
		std::for_each(
			begin(cmds),
			end(cmds),
			[&] (Rover::Command cmd) { rover.exec(cmd); }
		);

		out << rover << std::endl;
	}
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