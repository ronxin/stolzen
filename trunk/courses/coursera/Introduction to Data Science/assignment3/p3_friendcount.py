import MapReduce
import sys

import itertools 
import operator

mr = MapReduce.MapReduce()


def mapper(record):
  friend1 = record[0]
  friend2 = record[1]
  mr.emit_intermediate(friend1, friend2)

def reducer(key, list_of_values):
  mr.emit((key, len(list_of_values)))

if __name__ == '__main__':
  inputdata = open(sys.argv[1])
  mr.execute(inputdata, mapper, reducer)