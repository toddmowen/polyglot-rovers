// Simple assert-based tests (avoid dependency on NUnit or other third-party tool).


#include <assert.h>
#include <vec2_.h>


void testConstructVec2()
{
	rovers::Vec2 vec(4,7);

	assert(4 == vec.x());
	assert(7 == vec.y());
}


void testIncrement()
{
	rovers::Vec2 vec(4,7);
	vec += rovers::Vec2(-5, 6);

	assert(-1 == vec.x());
	assert(13 == vec.y());
}


void testRotateRight()
{
	rovers::Vec2 vec(4,7);
	vec.rotateRight();

	assert(rovers::Vec2(7,-4) == vec);
}


void testRotateLeft()
{
	rovers::Vec2 vec(4,7);
	vec.rotateLeft();

	assert(rovers::Vec2(-7,4) == vec);
}


void testVec2()
{
	testConstructVec2();
	testIncrement();
	testRotateRight();
	testRotateLeft();
}