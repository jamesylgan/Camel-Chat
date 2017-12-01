#!/bin/bash
#nc 127.0.0.1 9999 < test.txt 

test | make HOST="127.0.0.1" PORT=9999 tclient 