#include "rovers.h"

namespace rovers
{

// bearings
const Bearing Bearing::N('N', 0, 1);
Bearing const& N = Bearing::N;


// commands
void M(Rover& rover) { rover.x += rover.bearing.dx; rover.y += rover.bearing.dy; }


Bearing::Bearing(char symbol, int dx, int dy) : symbol(symbol), dx(dx), dy(dy)
{
}


Rover::Rover(int x, int y, Bearing const& bearing) : x(x), y(y), bearing(bearing)
{
}


bool Rover::operator==(Rover const& rover) const
{
	return this->x == rover.x
		&& this->y == rover.y
		&& &this->bearing == &rover.bearing;
}


}