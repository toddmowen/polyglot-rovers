#include <rovers.h>


namespace rovers
{


Vec2::Vec2(int x, int y)
	: x_(x), y_(y)
{
}


int
Vec2::x() const
{
	return x_;
}


int
Vec2::y() const
{
	return y_;
}


} // namespace rovers