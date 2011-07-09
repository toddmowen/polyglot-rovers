#!/bin/bash -e
#
# Runs the rovers program in its jar-packaged form, and verifies the output.
# (Contains multiple tests, but the "-e" flag above means that any failure will cause
# an immediate exit).

if [ $# -ne 1 ] ; then
	echo "Usage: testjar.sh JARFILE" 1>&2 
	exit 1
fi

JARFILE="$1"

# specify a file on the command line (in this case it is the device file, "/dev/fd/0")
COMMAND="java -jar $JARFILE /dev/fd/0"
echo "Running $COMMAND"
$COMMAND <<-INPUT_END | diff - /dev/fd/3 3<<-EXPECTED_OUTPUT_END
	5 5
	1 2 N
	LMLMLMLMM
	3 3 E
	MMRMMRMRRM
INPUT_END
	1 3 N
	5 1 E
EXPECTED_OUTPUT_END

# specify "-" for the input file
COMMAND="java -jar $JARFILE -"
echo "Running $COMMAND"
$COMMAND <<-INPUT_END | diff - /dev/fd/3 3<<-EXPECTED_OUTPUT_END
	4 2
	4 2 S
	MMRMMMMLLLM
INPUT_END
	0 1 N
EXPECTED_OUTPUT_END
