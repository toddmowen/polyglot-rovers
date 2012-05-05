#include <vector>

namespace rovers
{


class Vec2
{
public:
	Vec2(int x, int y);
	int x() const;
	int y() const;

	bool operator==(const Vec2&) const;
	void operator+=(const Vec2&);
	void rotateRight();  // rotate 90 degrees clockwise about origin
	void rotateLeft();   // rotate 90 degrees anti-clockwise about origin

private:
	void setXY(int x, int y);

	int x_, y_;
};


class Rover
{
public:
	Rover(int x, int y, Vec2 const& velocity);
	int x() const;
	int y() const;
	Vec2 velocity() const;

	// rover commands
	void M();
	void L();
	void R();

	// convenience method for using pointers to rover commands
	typedef void (Rover::*Command)(void);
	void exec(Command);

private:
	Vec2 position_;
	Vec2 velocity_;
};


}  // namespace rovers