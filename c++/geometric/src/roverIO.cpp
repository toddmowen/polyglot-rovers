#include <algorithm>
#include <utility>
#include <string>
#include <rovers.h>

namespace rovers
{


const std::pair<Vec2,char> CompassHeadings[] = {
	std::pair<Vec2,char>(Rover::EAST,  'E'),
	std::pair<Vec2,char>(Rover::NORTH, 'N'),
	std::pair<Vec2,char>(Rover::WEST,  'W'),
	std::pair<Vec2,char>(Rover::SOUTH, 'S')
};


char headingToChar(Vec2 const& heading)
{
	auto iter = std::find_if(
		begin(CompassHeadings),
		end(CompassHeadings),
		[&] (std::pair<Vec2,char> const& trans) { return trans.first == heading; }
	);

	if (end(CompassHeadings) == iter)
	{
		throw std::exception("Could not translate Vec2 to compass heading.");
	}

	return iter->second;
}


Vec2 charToHeading(const char c)
{
	auto iter = std::find_if(
		begin(CompassHeadings),
		end(CompassHeadings),
		[&] (std::pair<Vec2,char> const& trans) { return trans.second == c; }
	);

	if (end(CompassHeadings) == iter)
	{
		throw std::exception("Not a valid compass heading.");
	}

	return iter->first;
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


const std::pair<char,Rover::Command> Commands[] = {
	std::pair<char,Rover::Command>('L', &Rover::L),
	std::pair<char,Rover::Command>('R', &Rover::R),
	std::pair<char,Rover::Command>('M', &Rover::M)
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
		throw std::exception("Not a valid command.");
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