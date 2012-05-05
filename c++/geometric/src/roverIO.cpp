#include <algorithm>
#include <utility>
#include <string>
#include <sstream>
#include <rovers.h>

namespace rovers
{


// Store translation table as an association list (rather than a map)
// so that we can easily implement lookups in both directions.
const std::pair<char,Vec2> CompassHeadings[] = {
	std::make_pair('E', Rover::EAST),
	std::make_pair('N', Rover::NORTH),
	std::make_pair('W', Rover::WEST),
	std::make_pair('S', Rover::SOUTH)
};


char headingToChar(Vec2 const& heading)
{
	auto iter = std::find_if(
		begin(CompassHeadings),
		end(CompassHeadings),
		[&] (std::pair<char,Vec2> const& assoc) { return assoc.second == heading; }
	);

	if (end(CompassHeadings) == iter)
	{
		throw std::exception("Could not translate Vec2 to compass heading.");
	}

	return iter->first;
}


Vec2 charToHeading(const char c)
{
	auto iter = std::find_if(
		begin(CompassHeadings),
		end(CompassHeadings),
		[&] (std::pair<char,Vec2> const& trans) { return trans.first == c; }
	);

	if (end(CompassHeadings) == iter)
	{
		std::stringstream msg;
		msg << "Not a valid compass heading: " << c;
		throw std::exception(msg.str().c_str());
	}

	return iter->second;
}


std::ostream& operator<<(std::ostream& out, Rover const& rover)
{
	out << rover.x() << " " << rover.y() << " " << headingToChar(rover.heading());

	return out;
}


std::istream& operator>>(std::istream& is, Rover& rover)
{
	int x, y;
	char headingChar;

	is >> x >> y >> headingChar;
	Vec2 heading = charToHeading(headingChar);
	rover = Rover(x, y, heading);

	return is;
}


// Store translation table as an association list (rather than a map)
// for consistency with CompassHeadings table.
const std::pair<char,Rover::Command> Commands[] = {
	std::make_pair('L', &Rover::L),
	std::make_pair('R', &Rover::R),
	std::make_pair('M', &Rover::M)
};


Rover::Command charToCommand(const char c)
{
	auto iter = std::find_if(
		begin(Commands),
		end(Commands),
		[&] (std::pair<char,Rover::Command> const& assoc) { return assoc.first == c; }
	);

	if (end(Commands) == iter)
	{
		std::stringstream msg;
		msg << "Not a valid command: " << c;
		throw std::exception(msg.str().c_str());
	}

	return iter->second;
}


std::istream& operator>>(std::istream& is, std::vector<Rover::Command>& cmds)
{
	std::string chars;
	is >> chars;

	cmds.clear();
	std::for_each(
		begin(chars),
		end(chars),
		[&] (char c) { cmds.push_back(charToCommand(c)); }
	);

	return is;
}


} // namespace rovers