#!/usr/bin/python
# -*- coding: utf-8 -*-

import subprocess
import sys
import tempfile
from glob import glob


def solveIt(inputData):
    with tempfile.TemporaryFile("w+") as dataFile:
        dataFile.write(inputData)
        dataFile.seek(0)
        libs = ';'.join(glob('./lib/*'))
        return subprocess.check_output(['java', '-cp', libs, 'optimization.coloring.IndSetSolver'], shell=True, 
            stdin=dataFile)


if __name__ == '__main__':
    if len(sys.argv) > 1:
        fileLocation = sys.argv[1].strip()
        inputDataFile = open(fileLocation, 'r')
        inputData = ''.join(inputDataFile.readlines())
        inputDataFile.close()
        print solveIt(inputData)
    else:
        print 'This test requires an input file. (e.g python solver.py ./data/ks_4_0)'
