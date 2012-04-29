#include "rovers.h"

namespace rovers
{


class _Bearing {};

const _Bearing _N; const Bearing N(&_N);


Rover::Rover(int x, int y, Bearing bearing) : x(x), y(y), bearing(bearing)
{
}


Bearing::Bearing(const _Bearing* _bearing) : _bearing(_bearing)
{
}


bool Bearing::operator==(const Bearing& bearing) const
{
	// Because only the four pre-defined instances of _Bearing should ever exist,
	// it is adequate to just compare them by reference.
	return this->_bearing == bearing._bearing;
}

}