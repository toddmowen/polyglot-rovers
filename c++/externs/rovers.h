#ifndef ROVERS_H
#define ROVERS_H

namespace rovers
{


class _Bearing;

class Bearing
{
public:
	bool operator==(const Bearing& bearing) const;

private:
	_Bearing* _bearing;
};

extern Bearing N;


class Rover
{
public:
	int x, y;
	Bearing bearing;

	Rover(int x, int y, Bearing bearing);
};


}

#endif // ROVERS_H