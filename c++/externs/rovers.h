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
	Bearing const& turned(int quartersRight) const;

	const char symbol;
	const int dx, dy;

	static const Bearing N;
	static const Bearing E;
	static const Bearing S;
	static const Bearing W;

private:
	Bearing(const Bearing&);
	Bearing& operator=(const Bearing&);
};


// For convenience, the bearing constants are copied to the "rovers" namespace,
// so there is no need to write Bearing::N, etc.
extern Bearing const& N;
extern Bearing const& E;
extern Bearing const& S;
extern Bearing const& W;


class Rover
{
public:
	int x, y;
	Bearing const& bearing;

	Rover(int x, int y, Bearing const& bearing);
	bool operator==(Rover const& rover) const;
};


// Rover commands
typedef void (*command_t)(Rover&);
void M(Rover&);
void R(Rover&);


}

#endif // ROVERS_H