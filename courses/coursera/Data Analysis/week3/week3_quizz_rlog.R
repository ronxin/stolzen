#
# question 3, hclust
#

library(datasets)
data(iris)

irisSubset = iris[, 1:4]

irisDist = dist(irisSubset)
irisTree = hclust(irisDist)

plot(irisTree)

# we want to see how many clusters are at the height of 3
rect.hclust(irisTree, h=3, border="red")

# to cut the tree, we may use...
f = cutree(irisTree, h=3)

# indeed, there are 4 clusters
table(f)

#
# question 4
#
download.file("https://spark-public.s3.amazonaws.com/dataanalysis/quiz3question4.rda", 
              destfile="quiz3question4.rda")

load("quiz3question4.rda")
# reads to "dataSet" variable

# note 2 clusters
plot(dataSet, pch=19, col="blue")

ds.kmeans = kmeans(dataSet, centers=2)

# let's draw the clusters
plot(dataSet, pch=19, col=ds.kmeans$cluster)
points(ds.kmeans$centers, col=1:2, pch=4, cex=4)


#
# question 5
#

if (!require("ElemStatLearn")) {
  install.packages("ElemStatLearn")
  library(ElemStatLearn)
}
data(zip.train)

# Create an image matrix for the 3rd row, which is a 4
im = zip2image(zip.train, 3)
image(im)

# 8th row (1)
im8 = zip2image(zip.train, 8)
svd.im8 = svd(im8)

par(mfrow=c(2, 3))
image(im8)
plot(svd.im8$d, xlab="Column", ylab="Singluar value", pch=19)
plot(svd.im8$d ^ 2 / sum(svd.im8$d ^ 2), xlab="Column", 
     ylab="Percent of variance explained", pch=19)

svd.im8.var = svd.im8$d ^ 2 / sum(svd.im8$d ^ 2)
svd.im8.var[1]


# 18th row (8)
im18 = zip2image(zip.train, 18)


svd.im18 = svd(im18)
image(im18)
plot(svd.im18$d, xlab="Column", ylab="Singluar value", pch=19)
plot(svd.im18$d ^ 2 / sum(svd.im18$d ^ 2), xlab="Column", 
     ylab="Percent of variance explained", pch=19)

svd.im18.var = svd.im18$d ^ 2 / sum(svd.im18$d ^ 2)
svd.im18.var

# 18th row is more complicated so there are multiple patterns each explaining
# a large percentile of variance