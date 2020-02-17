#!/bin/bash

cd "$(dirname "$(readlink -f $0)")"

NC='\033[0m'
CYAN='\033[0;36m'
RED='\033[0;31m'
GREEN='\033[0;32m'

RHEA="../target/debug/rhea"

SILENT=true

testdir=$1

if [ -d $testdir ]; then
	[ -z $SILENT ] && printf "${CYAN}test:${NC} $(basename $testdir)\n"

	$RHEA --jit -O --input "$testdir/code.rhea" 2>&1 | diff "$testdir/expected_output.txt" -
	if [ $? -ne 0 ]; then
		[ -z $SILENT ] && printf "${RED}wrong exit code or output (${testdir})${NC}\n"
		exit 1
	fi
else
	exit 1
fi

[ -z $SILENT ] && printf "${GREEN}SUCCESS:${NC} Test passed!\n"

exit 0

