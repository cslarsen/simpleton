#! /bin/bash
python trans.py $* | g++ -xc++ -
