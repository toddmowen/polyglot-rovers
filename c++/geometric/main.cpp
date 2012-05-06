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
	// Ignore plateau bounds (not used in the basic solution to the problem)
	skipLine(in);

	Rover rover;
	std::vector<Rover::Command> cmds;
	while (in >> rover >> cmds)
	{
		std::for_each(
			begin(cmds),
			end(cmds),
			[&] (Rover::Command cmd) { rover.exec(cmd); }
		);

		out << rover << std::endl;
	}

	if (!in.eof())
	{
		throw std::exception("Parse error");
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