import MapReduce
import sys

mr = MapReduce.MapReduce()

def mapper(record):
  person = record[0]
  friend = record[1]
  mr.emit_intermediate('%s:%s' % (person, friend), (person, friend))
  mr.emit_intermediate('%s:%s' % (friend, person), (friend, person))

def reducer(key, list_of_values):
  if (len(list_of_values) == 1):
    mr.emit(list_of_values[0])

if __name__ == '__main__':
  inputdata = open(sys.argv[1])
  mr.execute(inputdata, mapper, reducer)