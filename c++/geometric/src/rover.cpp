#include <rovers.h>


namespace rovers
{


// heading constants
const Vec2 Rover::EAST()  { return Vec2( 1, 0); }
const Vec2 Rover::NORTH() { return Vec2( 0, 1); }
const Vec2 Rover::WEST()  { return Vec2(-1, 0); }
const Vec2 Rover::SOUTH() { return Vec2( 0,-1); }

	
Rover::Rover(const int x, const int y, Vec2 const& heading)
	: position_(Vec2(x,y)), heading_(heading)
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
Rover::heading() const
{
	return heading_;
}


bool
Rover::operator==(Rover const& that) const
{
	return (position_ == that.position_) && (heading_ == that.heading_);
}


void
Rover::M()
{
	position_ += heading_;
}


void
Rover::L()
{
	heading_.rotateLeft();
}


void
Rover::R()
{
	heading_.rotateRight();
}


void
Rover::exec(const Rover::Command cmd)
{
	(this->*cmd)();
}


} // namespace rovers