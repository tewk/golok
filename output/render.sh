#!/bin/bash

for i in $(ls output/*.dot 2>/dev/null)
do
	if [ ! -e ${i%dot}svg ]
	then
		echo "Building ${i%.dot}..."
		dot -Tsvg -o ${i%dot}svg $i
	else
		echo "skipping ${i%.dot}..."
	fi
done
