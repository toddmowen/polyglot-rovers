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


std::ostream& operator<<(std::ostream& out, Rover const& rover)
{
	out << rover.x() << " " << rover.y() << " " << headingToChar(rover.heading());
	return out;
}


} // namespace rovers