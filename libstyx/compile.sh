#!/bin/bash

gcc -fpic -c styx.c -lsystemd -lssl -lcrypto &&
    gcc -shared -o libstyx.so styx.o -lsystemd -lssl -lcrypto &&
    cp libstyx.so ~/.lisp
