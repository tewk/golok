#!/bin/bash

# pack up for a binary distribution

NAME=""

if [[ -z $1 ]]
then
  NAME="support";
else
NAME=golok-bin-v$1;
fi

make

mzc --exe-dir $NAME golok

mkdir $NAME/examples
cp examples/*.amf $NAME/examples/
cp README $NAME/

tar cjvf $NAME.tar.bz2 $NAME

rm -rf $NAME