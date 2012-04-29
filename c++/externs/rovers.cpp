#include "rovers.h"

namespace rovers
{


const Bearing Bearing::N('N', 0, 1);
Bearing const& N = Bearing::N;


Bearing::Bearing(char symbol, int dx, int dy) : symbol(symbol), dx(dx), dy(dy)
{
}


Rover::Rover(int x, int y, Bearing const& bearing) : x(x), y(y), bearing(bearing)
{
}


}