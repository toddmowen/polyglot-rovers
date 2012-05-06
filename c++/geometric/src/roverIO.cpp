#include <algorithm>
#include <utility>
#include <string>
#include <sstream>
#include <rovers.h>

namespace rovers
{


// Store translation table as an association list (rather than a map)
// so that we can use the same structure for lookups in both directions.
const std::pair<char,Vec2>
HEADINGS[] =
{
	std::make_pair('E', Rover::EAST()),
	std::make_pair('N', Rover::NORTH()),
	std::make_pair('W', Rover::WEST()),
	std::make_pair('S', Rover::SOUTH())
};


char
headingToChar(Vec2 const& heading)
{
	for (auto iter = begin(HEADINGS); iter != end(HEADINGS); iter++)
	{
		if (iter->second == heading)
		{
			return iter->first;
		}
	}

	throw std::exception("Could not translate Vec2 to compass heading.");
}


Vec2
charToHeading(const char c)
{
	for (auto iter = begin(HEADINGS); iter != end(HEADINGS); iter++)
	{
		if (iter->first == c)
		{
			return iter->second;
		}
	}

	std::stringstream msg;
	msg << "Not a valid compass heading: " << c;
	throw std::exception(msg.str().c_str());
}


std::ostream&
operator<<(std::ostream& out, Rover const& rover)
{
	out << rover.x() << " " << rover.y() << " " << headingToChar(rover.heading());

	return out;
}


std::istream&
operator>>(std::istream& is, Rover& rover)
{
	int x, y;
	char headingChar;

	if (is >> x >> y >> headingChar)
	{
		Vec2 heading = charToHeading(headingChar);
		rover = Rover(x, y, heading);
	}

	return is;
}


// Store translation table as an association list (rather than a map)
// for consistency with HEADINGS table.
const std::pair<char,Rover::Command>
COMMANDS[] =
{
	std::make_pair('L', &Rover::L),
	std::make_pair('R', &Rover::R),
	std::make_pair('M', &Rover::M)
};


Rover::Command
charToCommand(const char c)
{
	for (auto iter = begin(COMMANDS); iter != end(COMMANDS); iter++)
	{
		if (iter->first == c)
		{
			return iter->second;
		}
	}

	std::stringstream msg;
	msg << "Not a valid command: " << c;
	throw std::exception(msg.str().c_str());
}


std::istream&
operator>>(std::istream& is, std::vector<Rover::Command>& cmds)
{
	std::string chars;

	cmds.clear();
	if (is >> chars)
	{
		std::for_each(
			begin(chars),
			end(chars),
			[&] (char c) { cmds.push_back(charToCommand(c)); }
		);
	}

	return is;
}


} // namespace rovers