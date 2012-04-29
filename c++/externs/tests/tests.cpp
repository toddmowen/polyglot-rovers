/*
 * Rather than introducing a dependency on NUnit or some other test framework,
 * just use some simple assert-based tests.
 */

#include <iostream>
#include <iterator>
#include <assert.h>

#include "../rovers.h"


using namespace std;
using namespace rovers;


void test_construct()
{
	Rover rover(1, 2, N);

	assert(1 == rover.x);
	assert(2 == rover.y);
	assert(N == rover.bearing);
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


void test_L()
{
	Rover rover(1, 2, N);
	L(rover);

	assert(Rover(1, 2, W) == rover);
}


void test_exec()
{
	Rover rover(1, 2, N);
	command_t cmds[] = {L, M, L, M, L, M, L, M, M};
	rover.exec(begin(cmds), end(cmds));

	assert(Rover(1, 3, N) == rover);
}


int main()
{
	test_construct();
	test_M();
	test_R();
	test_L();
	test_exec();

	cout << "All tests passed!" << endl;
	return 0;
}