#!/bin/bash


wget http://www.nsnam.org/release/ns-allinone-3.39.tar.bz2
tar xjf ns-allinone-3.39.tar.bz2
cd ns-allinone-3.39
./build.py
cd ns-3.39
./test.py

