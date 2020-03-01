#!/bin/bash

cd "$(dirname "$(readlink -f $0)")"

cd ..
cargo build --quiet
cd ./examples

NC='\033[0m'
CYAN='\033[0;36m'
RED='\033[0;31m'
GREEN='\033[0;32m'

RHEA="../target/debug/rhea"

if [ "$1" = "--silent" ]; then
	SILENT=true
fi

i=0
for dir in ./*/; do
	if [ -d $dir ] && [ -f "$dir/expected_output.txt" ]; then
		i=$(($i + 1))
		[ -z $SILENT ] && printf "${CYAN}test #${i}:${NC} $(basename $dir)\n"

		$RHEA --jit --input "$dir/code.rhea" 2>&1 | diff "$dir/expected_output.txt" -
		if [ $? -ne 0 ]; then
			[ -z $SILENT ] && printf "${RED}wrong exit code or output (${dir})${NC}\n"
			exit 1
		fi
	fi
done

[ -z $SILENT ] && printf "${GREEN}SUCCESS:${NC} All Tests passed!\n"

exit 0

