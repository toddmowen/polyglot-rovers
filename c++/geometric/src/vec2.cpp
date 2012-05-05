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


} // namespace rovers