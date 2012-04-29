#ifndef ROVERS_H
#define ROVERS_H

#include <algorithm>


namespace rovers
{

class Bearing;
typedef Bearing const* bearing_t;
class Rover;
typedef void (*command_t)(Rover&);

// Bearings
extern const bearing_t N, E, S, W;

// Rover commands
void M(Rover&);
void R(Rover&);
void L(Rover&);


class Rover
{
public:
	int x, y;
	bearing_t bearing;

	Rover(int x, int y, bearing_t bearing);
	bool operator==(Rover const& rover) const;

	// Overloaded exec() function takes either a single command_t, or an iterator range.
	void exec(command_t);
	template<class InputIterator> void exec(InputIterator first, InputIterator last)
	{
		std::for_each(first, last, [&](command_t cmd) { this->exec(cmd); } );
	}
};


/*
 * Variation of the type-safe enum pattern: the four cardinal compass directions
 * are the only four instances of this class. The bearing_t constants declared
 * above are simply pointers to these unique instances.
 */
class Bearing
{
public:
	Bearing(char symbol, int dx, int dy);
	bearing_t turned(int quartersRight) const;

	const char symbol;
	const int dx;
	const int dy;

	static const Bearing _N;
	static const Bearing _E;
	static const Bearing _S;
	static const Bearing _W;

private:
	// hide default copy constructor and assignment operator
	Bearing(const Bearing&);
	Bearing& operator=(const Bearing&);
};


}

#endif // ROVERS_H