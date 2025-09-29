#!/usr/bin/bash

if [ ! -d "../build" ]; then
    mkdir ../build > /dev/null
fi

pushd ../build > /dev/null

compiler_flags="-O0 -DBUILD_DEBUG"
include_flags="-I$HOME/projects/core/src/core"

g++ $include_flags $compiler_flags -o ./arc ../src/main.cpp

pushd ../data > /dev/null
../build/arc ./test_01.arc

popd > /dev/null
popd > /dev/null




