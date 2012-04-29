#include <algorithm>
#include "rovers.h"

using namespace std;

namespace rovers
{

// bearings
const Bearing Bearing::_N('N',  0,  1);  const bearing_t N = &Bearing::_N;
const Bearing Bearing::_E('E',  1,  0);  const bearing_t E = &Bearing::_E;
const Bearing Bearing::_S('S',  0, -1);  const bearing_t S = &Bearing::_S;
const Bearing Bearing::_W('W', -1,  0);  const bearing_t W = &Bearing::_W;


// commands
void M(Rover& rover) { rover.x += rover.bearing->dx; rover.y += rover.bearing->dy; }
void R(Rover& rover) { rover.bearing = rover.bearing->turned(1); }
void L(Rover& rover) { rover.bearing = rover.bearing->turned(-1); }


Bearing::Bearing(char symbol, int dx, int dy) : symbol(symbol), dx(dx), dy(dy)
{
}


bearing_t Bearing::turned(int quartersRight) const
{
	static bearing_t order[] = {N, E, S, W};
	static auto begin = &order[0];
	static auto end = &order[4];

	int this_idx = find(begin, end, this) - begin;
	int turn_idx = (this_idx + quartersRight) % 4;
	if (turn_idx < 0) turn_idx += 4;  // deal with possible negative result of '%'

	return order[turn_idx];
}


Rover::Rover(int x, int y, bearing_t bearing) : x(x), y(y), bearing(bearing)
{
}


bool Rover::operator==(Rover const& rover) const
{
	return this->x == rover.x
		&& this->y == rover.y
		&& this->bearing == rover.bearing;
}


}