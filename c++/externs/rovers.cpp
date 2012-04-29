#include <algorithm>
#include "rovers.h"

namespace rovers
{

// bearings
const Bearing Bearing::N('N',  0,  1);  Bearing const& N = Bearing::N;
const Bearing Bearing::E('E',  1,  0);  Bearing const& E = Bearing::E;
const Bearing Bearing::S('S',  0, -1);  Bearing const& S = Bearing::S;
const Bearing Bearing::W('W', -1,  0);  Bearing const& W = Bearing::W;


// commands
void M(Rover& rover) { rover.x += rover.bearing.dx; rover.y += rover.bearing.dy; }
void R(Rover& rover) { rover.bearing = rover.bearing.turned(1); }


Bearing::Bearing(char symbol, int dx, int dy) : symbol(symbol), dx(dx), dy(dy)
{
}


Bearing const& Bearing::turned(int quartersRight)
{
	static auto order = {N, E, S, W};
	static auto begin = &order[0];
	static auto end = &order[4];

	int this_idx = find(begin, end, *this) - begin;
	int turn_idx = (this_idx + quartersRight) % 4;
	if (turn_idx < 0) turn_idx += 4;

	return order[turn_idx];
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