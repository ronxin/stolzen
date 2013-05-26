import MapReduce
import sys

mr = MapReduce.MapReduce()

# A: m x k; B: k x n
# C: m x n
m = 5
k = 5
n = 5 

def mapper(record):
  matrix = record[0]
  i = record[1]
  j = record[2]
  if matrix == 'a':
    for x in range(0, k):
      mr.emit_intermediate((i, x), record)
  else:
    for x in range(0, k):
      mr.emit_intermediate((x, j), record)

def reducer(key, list_of_values):
  matrix_a = filter(lambda x: x[0] == 'a', list_of_values)
  a = [0] * k
  for el_a in matrix_a:
    j = el_a[2]
    value = el_a[3]
    a[j] = value

  matrix_b = filter(lambda x: x[0] == 'b', list_of_values)
  b = [0] * k
  for el_b in matrix_b:
    i = el_b[1]
    value = el_b[3]
    b[i] = value  

  res = sum([x1 * x2 for (x1, x2) in zip(a, b)])
  if (res != 0):
    mr.emit((key[0], key[1], res)) 

if __name__ == '__main__':
  inputdata = open(sys.argv[1])
  mr.execute(inputdata, mapper, reducer)