#ifndef ROVERS_H
#define ROVERS_H

namespace rovers
{

class Bearing;
typedef Bearing const* bearing_t;
class Rover;
typedef void (*command_t)(Rover&);

// Bearings
extern bearing_t N, E, S, W;

// Rover commands
void M(Rover&);
void R(Rover&);
void L(Rover&);


/*
 * Type-safe enum pattern.
 * Apart from the four predefined directions, other Bearing instances cannot be
 * created. To test for equality, please compare by reference.
 */
class Bearing
{
public:
	Bearing(char symbol, int dx, int dy);
	bearing_t turned(int quartersRight) const;

	const char symbol;
	const int dx, dy;

	static const Bearing _N;
	static const Bearing _E;
	static const Bearing _S;
	static const Bearing _W;

private:
	Bearing(const Bearing&);
	Bearing& operator=(const Bearing&);
};


class Rover
{
public:
	int x, y;
	bearing_t bearing;

	Rover(int x, int y, bearing_t bearing);
	bool operator==(Rover const& rover) const;
};


}

#endif // ROVERS_H