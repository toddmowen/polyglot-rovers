#include <rovers.h>


namespace rovers
{


// bearing constants
const Vec2 Rover::EAST  ( 1, 0);
const Vec2 Rover::NORTH ( 0, 1);
const Vec2 Rover::WEST  (-1, 0);
const Vec2 Rover::SOUTH ( 0,-1);

	
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


void
Rover::M()
{
	position_ += velocity_;
}


void
Rover::L()
{
	velocity_.rotateLeft();
}


void
Rover::R()
{
	velocity_.rotateRight();
}


void
Rover::exec(const Rover::Command cmd)
{
	(this->*cmd)();
}


} // namespace rovers