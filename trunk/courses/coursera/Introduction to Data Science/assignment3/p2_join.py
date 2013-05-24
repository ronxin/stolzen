import MapReduce
import sys

import itertools 
import operator

mr = MapReduce.MapReduce()


def mapper(record):
  id = record[1]
  mr.emit_intermediate(id, record)

def reducer(key, list_of_values):
  grouped = itertools.groupby(list_of_values, operator.itemgetter(0))
  g = {k: list(v) for (k, v) in grouped}
  order = g['order'][0]

  for line_item in g['line_item']:
    mr.emit(order + line_item)

if __name__ == '__main__':
  inputdata = open(sys.argv[1])
  mr.execute(inputdata, mapper, reducer)