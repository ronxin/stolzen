import MapReduce
import sys

mr = MapReduce.MapReduce()

def mapper(record):
  key = record[0]
  words = record[1].split()
  for word in set(words):
    mr.emit_intermediate(word, key)

def reducer(key, list_of_values):
  unique = list(set(list_of_values))
  mr.emit((key, list_of_values))

if __name__ == '__main__':
  inputdata = open(sys.argv[1])
  mr.execute(inputdata, mapper, reducer)
