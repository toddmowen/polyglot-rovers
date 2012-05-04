// Simple assert-based tests (avoid dependency on NUnit or other third-party tool).


#include <assert.h>
#include <rovers.h>


void testConstructVec2()
{
	rovers::Vec2 vec(4,7);

	assert(4 == vec.x());
	assert(7 == vec.y());
}


void testVec2()
{
	testConstructVec2();
}