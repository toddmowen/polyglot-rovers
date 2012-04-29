#ifndef ROVERS_H
#define ROVERS_H

namespace rovers
{


/*
 * Type-safe enum pattern.
 * Apart from the four predefined directions, other Bearing instances cannot be
 * created. To test for equality, please compare by reference.
 */
class Bearing
{
public:
	Bearing(char symbol, int dx, int dy);

	const char symbol;
	const int dx, dy;

	static const Bearing N;

private:
	Bearing(const Bearing&);
	Bearing& operator=(const Bearing&);
};


// For convenience, the bearing constants are copied to the "rovers" namespace,
// so there is no need to write Bearing::N, etc.
extern Bearing const& N;


class Rover
{
public:
	int x, y;
	Bearing const& bearing;

	Rover(int x, int y, Bearing const& bearing);
};


}

#endif // ROVERS_H