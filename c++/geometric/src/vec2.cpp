#include <rovers.h>


namespace rovers
{


Vec2::Vec2(const int x, const int y)
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


bool
Vec2::operator==(const Vec2& that) const
{
	return (x_ == that.x_) && (y_ == that.y_);
}


bool
Vec2::operator!=(const Vec2& that) const
{
	return !(*this == that);
}


void
Vec2::operator+=(const Vec2& that)
{
	setXY(
		x_ + that.x_,
		y_ + that.y_
	);
}


void
Vec2::rotateRight()
{
	setXY(
		y_,
		-x_
	);
}


void
Vec2::rotateLeft()
{
	setXY(
		-y_,
		x_
	);
}


void
Vec2::setXY(const int x, const int y)
{
	x_ = x;
	y_ = y;
}


} // namespace rovers