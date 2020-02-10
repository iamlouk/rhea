#!/bin/bash

cd "$(dirname "$(readlink -f $0)")"

cd ..
cargo build
cd ./examples

NC='\033[0m'
CYAN='\033[0;36m'
RED='\033[0;31m'
GREEN='\033[0;32m'

RHEA="../target/debug/rhea"

i=0
for dir in ./*/; do
	if [ -d $dir ]; then
		i=$(($i + 1))
		printf "${CYAN}test #${i}:${NC} $(basename $dir)\n"

		$RHEA --jit --input "$dir/code.rhea" | diff "$dir/expected_output.txt" -
		if [ $? -ne 0 ]; then
			printf "${RED}wrong exit code or output (test: $dir)${NC}\n"
			exit 1
		fi

		# diff "$dir/expected_output.txt" "$dir/actual_output.txt"
		# if [ $? -ne 0 ]; then
		#	printf "${RED}wrong output (test; $dir)${NC}\n"
		#	exit 1
		# fi
	fi
done

printf "${GREEN}SUCCESS:${NC} All Tests passed!\n"

