# 2
set.seed(53535)
xValues = seq(0, 2*pi,length=100)
yValues = rnorm(100) + sin(xValues)

rmse = function(x, y) { 
  sqrt(mean((x - y) ^ 2)) 
}

library(splines)

rmses = rep(NA, 10)

for (i in 1:10) {
  ns1 = ns(yValues, df=i)
  lm1 = lm(yValues ~ ns1)
  rmses[i] = rmse(yValues, lm1$fitted)
}

plot(rmses)
lines(rmses)

# 3
install.packages("simpleboot")
library(simpleboot) 
data(airquality)
attach(airquality)

b.fun <- function(data, indices) { 
  quantile(data[indices], 0.75)
}

set.seed(883833)
boot1 = one.boot(airquality$Wind, b.fun, R = 1000)
median(boot1$t[, 1])
sd(boot1$t[, 1])

# 4
library(tree)
data(Cars93, package="MASS")
set.seed(7363)

tree1 <- tree(DriveTrain ~ Price + Type, data=Cars93[sample(1:dim(Cars93)[1]),])
tree2 <- tree(DriveTrain ~ Price + Type, data=Cars93[sample(1:dim(Cars93)[1]),])
tree3 <- tree(DriveTrain ~ Price + Type, data=Cars93[sample(1:dim(Cars93)[1]),])

testData = data.frame(Type="Large", Price=20)

predict(tree1, data=testData, type="class")
predict(tree2, data=testData, type="class")
predict(tree3, data=testData, type="class")

# 5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 

vowel.train$y = factor(vowel.train$y)
vowel.test$y = factor(vowel.test$y)


set.seed(33833)

install.packages("e1071")
install.packages("randomForest")
library(e1071)
library(randomForest)

rf1 = randomForest(y ~ ., data=vowel.train)
svm1 = svm(y ~ ., data=vowel.train)
