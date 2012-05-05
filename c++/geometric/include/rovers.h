#ifndef GEOMETRIC_ROVERS_H_
#define GEOMETRIC_ROVERS_H_

#include <iostream>
#include <vector>
#include <vec2_.h>

namespace rovers
{


class Rover
{
public:
	Rover(int x, int y, Vec2 const& heading);
	int x() const;
	int y() const;
	Vec2 heading() const;
	bool operator==(Rover const&) const;

	// heading constants
	static const Vec2 EAST;
	static const Vec2 NORTH;
	static const Vec2 WEST;
	static const Vec2 SOUTH;

	// rover commands
	void M();
	void L();
	void R();

	// convenience method for using pointers to rover commands
	typedef void (Rover::*Command)(void);
	void exec(Command);

private:
	Vec2 position_;
	Vec2 heading_;
};


// stream-based IO
//
// Note: behaviour for negative coordinates is undefined (since according to the
// problem spec, the south-west corner of the plateau is always 0,0).

std::ostream& operator<<(std::ostream&, Rover const&);
std::istream& operator>>(std::istream&, Rover&);
std::istream& operator>>(std::istream&, std::vector<Rover::Command>&);


}  // namespace rovers

#endif  // GEOMETRIC_ROVERS_H_