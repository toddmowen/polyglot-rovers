#include <rovers.h>


namespace rovers
{


Rover::Rover(const int x, const int y, Vec2 const& velocity)
	: position_(Vec2(x,y)), velocity_(velocity)
{
}


int
Rover::x() const
{
	return position_.x();
}


int
Rover::y() const
{
	return position_.y();
}


Vec2
Rover::velocity() const
{
	return velocity_;
}


} // namespace rovers