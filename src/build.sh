#!/usr/bin/bash

if [ ! -d "../build" ]; then
    mkdir ../build  > /dev/null
fi


pushd ../build > /dev/null

compiler_flags="-O0 -DBUILD_DEBUG -g"
include_flags="-I$HOME/projects/core/src/core"

g++ $include_flags $compiler_flags -o ./arc ../src/main.cpp

pushd ../data  > /dev/null
../build/arc ./test_00.arc
../build/arc ./test_01.arc
../build/arc ./test_02.arc
../build/arc ./test_03.arc
../build/arc ./test_04.arc
../build/arc ./test_05.arc

popd > /dev/null

popd  > /dev/null




