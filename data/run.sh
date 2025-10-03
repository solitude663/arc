#!/usr/bin/bash

nasm -f elf64 ./out.nasm -o out.o
gcc ./out.o -o ./out -no-pie
./out
