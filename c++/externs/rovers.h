#ifndef ROVERS_H
#define ROVERS_H

namespace rovers
{


class Direction
{
};


auto N = Direction();


class Rover
{
public:
	int x, y;
	Direction direction;

	Rover(int x, int y, Direction direction);
};


}

#endif // ROVERS_H