#!/usr/bin/bash

if [ ! -d "../build" ]; then
    mkdir ../build  > /dev/null
fi

echo Current Directory
pwd
pushd ../build > /dev/null

echo New Directory
pwd


compiler_flags="-O0 -DBUILD_DEBUG -g"
include_flags="-I$HOME/projects/core/src/core"

g++ $include_flags $compiler_flags -o ./arc ../src/main.cpp

pushd ../data  > /dev/null
../build/arc ./test_01.arc
echo New Directory
pwd

popd > /dev/null
echo New Directory
pwd

popd  > /dev/null
echo New Directory
pwd




