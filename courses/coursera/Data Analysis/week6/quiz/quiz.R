# 3

library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

glm1 = glm(chd ~ age + alcohol + obesity + tobacco + typea + ldl, family="binomial", data=trainSA)

missClass = function(values, prediction){
  sum(((prediction > 0.5) * 1) != values) / length(values)
}

pred0 = predict(glm1, newdata=trainSA, type="response")
val0 = trainSA$chd
missClass(val0, pred0)

pred1 = predict(glm1, newdata=testSA, type="response")
val1 = testSA$chd
missClass(val1, pred1)


# 4
install.packages("pgmm")
library(pgmm)
data(olive)
olive = olive[,-1]

install.packages("tree")
library(tree)
tree1 = tree(Area ~ ., data=olive)
plot(tree1)
text(tree1)

newdata = as.data.frame(t(colMeans(olive)))
pred1 <- predict(tree1,newdata)
pred1

#5


