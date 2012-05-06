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
	Rover();
	int x() const;
	int y() const;
	Vec2 heading() const;
	bool operator==(Rover const&) const;

	// heading constants (implemented as static calls to avoid initialization issues)
	static const Vec2 EAST();
	static const Vec2 NORTH();
	static const Vec2 WEST();
	static const Vec2 SOUTH();

	// rover commands (to pass to exec(), take address of method with "&")
	void M();
	void L();
	void R();

	// convenience methods for using pointers to the above rover commands
	typedef void (Rover::*Command)(void);
	void exec(Command);
	void exec(std::vector<Command> const&);

private:
	Vec2 position_;
	Vec2 heading_;
};


// stream-based IO
//
// Note: These do not distinguish between spaces and newlines, therefore they
// accept a superset of the format described in the problem spec.

std::ostream& operator<<(std::ostream&, Rover const&);
std::istream& operator>>(std::istream&, Rover&);
std::istream& operator>>(std::istream&, std::vector<Rover::Command>&);


}  // namespace rovers

#endif  // GEOMETRIC_ROVERS_H_