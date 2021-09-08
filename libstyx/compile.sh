#!/bin/bash

gcc -fpic -c styx.c -lsystemd &&
    gcc -shared -o libstyx.so styx.o -lsystemd &&
    cp libstyx.so ~/.lisp
