#!/bin/bash

# this *should* be something nice to ensure stuff works

TESTS="dme dme-mult SpinLock-mult dpp lr-dpp"

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
  if [[ "$i" == "SpinLock" ]]
  then
  ./golok -o output --npp object examples/$i.amf || fail
  elif [[ "$i" == "producer-consumer" ]]
  then
  ./golok -o output --npp buffer examples/$i.amf || fail
  else
  ./golok -o output examples/$i.amf || fail
  fi
 
#echo "Testing DFS"
# ./golok -o output --dfs examples/$i.amf || fail
# make clean
done

make clean

echo "looks good!"
