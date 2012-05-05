#include <algorithm>
#include <utility>
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


Vec2 charToHeading(char c)
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


} // namespace rovers