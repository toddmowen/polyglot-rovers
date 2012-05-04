#include <assert.h>

// Simple assert-based tests (avoid dependency on NUnit or other third-party tool).


void testConstructVec2()
{
	Vec2 vec(4,7);

	assert(4 == vec.x);
	assert(7 == vec.y);
}


void testVec2()
{
	testConstructVec2();
}