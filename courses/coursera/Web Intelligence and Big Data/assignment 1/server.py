#!/usr/bin/env python

import glob
text_files = glob.glob("hw3data/*")

def file_content(file_name):
    f = open(file_name)
    try:
        return f.read()
    finally:
        f.close()

print "loading the data..."

source = dict((file_name, file_content(file_name))
            for file_name in text_files)

print "finished"

import assignment1


import mincemeat
s = mincemeat.Server()
s.datasource = source
s.mapfn = assignment1.mapfn
s.reducefn = assignment1.reducefn

results = s.run_server(password="changeme")


# Enter one of the top 2 terms used in a title by the author: Michael Stonebraker
# Enter one of the top 2 terms used in a title by the author: Grzerorz Rozenberg

output = open('res.txt', 'w')

output.write(repr(results))

print results['Michael Stonebraker']
print results['Grzegorz Rozenberg']