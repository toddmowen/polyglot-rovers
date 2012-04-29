#ifndef ROVERS_H
#define ROVERS_H

namespace rovers
{


class _Bearing;

class Bearing
{
public:
	Bearing(const _Bearing* _bearing);
	bool operator==(const Bearing& bearing) const;

private:
	const _Bearing* _bearing;
};


// Bearing constants
extern const Bearing N;


class Rover
{
public:
	int x, y;
	Bearing bearing;

	Rover(int x, int y, Bearing bearing);
};


}

#endif // ROVERS_H