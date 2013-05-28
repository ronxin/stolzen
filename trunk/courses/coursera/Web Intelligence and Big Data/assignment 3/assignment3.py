# data
# https://spark-public.s3.amazonaws.com/bigdata/EthinicityDataSet/genestrain.tab.zip
# https://spark-public.s3.amazonaws.com/bigdata/EthinicityDataSet/genesblind.tab.zip

# import os
# os.chdir('C:\\dev\\courses\\coursera\\Web Intelligence and Big Data\\assignment 3')

import Orange

data = Orange.data.Table("genestrain")
test = Orange.data.Table("genesblind")

learner = Orange.classification.bayes.NaiveLearner()
classifier = learner(data)

m = len(test)
res = [''] * m
for i in range(m):
    res[i] = classifier(test[i])

code = {'CEU': '0', 'GIH': '1', 'JPT': '2', 'ASW': '3', 'YRI': '4'}
print ' '.join([code[r] for r in res])