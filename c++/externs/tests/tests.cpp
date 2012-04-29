/*
 * Rather than introducing a dependency on NUnit or some other test framework,
 * just use some simple assert-based tests.
 */

#include <iostream>
#include <assert.h>

#include "../rovers.h"


using namespace std;
using namespace rovers;


void test_construct()
{
	Rover rover(1, 2, N);

	assert(1 == rover.x);
	assert(2 == rover.y);
	assert(&N == &rover.bearing);
}


void test_M()
{
	Rover rover(1, 2, N);
	M(rover);

	assert(Rover(1, 3, N) == rover);
}


void test_R()
{
	Rover rover(1, 2, N);
	R(rover);

	assert(Rover(1, 2, E) == rover);
}


int main()
{
	test_construct();
	test_M();
	test_R();

	cout << "All tests passed!" << endl;
	return 0;
}