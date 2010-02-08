#!/bin/bash

# this *should* be something nice to ensure stuff works

TESTS="dme dme-mult SpinLock"

make clean
make

fail () 
{
  make clean
  echo "Failed"
  exit 1
}

for i in $TESTS
do
echo "Testing BFS"
  ./golok -o output examples/$i.amf || fail
  make clean
 
echo "Testing DFS"
  ./golok -o output --dfs examples/$i.amf || fail
  make clean
done

make clean

echo "looks good!"
