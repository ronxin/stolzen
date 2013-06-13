dataset = read.csv("train.csv")

names(dataset)


sapply(dataset[1, ], class)
summary(dataset)

len = length(dataset[, 1])
len
length(which(complete.cases(dataset)))


incomplete = which(!complete.cases(dataset))
incomplete

# instead of NAs in age putting the avg. age there
mean.age = round(mean(na.omit(dataset$age)))
dataset$age[is.na(dataset$age)] = mean.age

incomplete = which(!complete.cases(dataset))
incomplete

# converting pclass into factor
dataset$pclass = as.factor(dataset$pclass)



# preparing for cross-validation
set.seed(31337)
train_ind = sample(1:len, size=0.6*len, replace=F)

# length(dataset[-train_ind, 1]) + length(dataset[train_ind, 1])
# length(dataset[, 1])

train = dataset[train_ind, ]
test = dataset[-train_ind, ]

precision = function(predicted, actual) {
  t = table(predicted, actual)
  true.negatives = t[1, 1]
  true.positives = t[2, 2]
  false.negatives = t[1, 2]
  false.positives = t[2, 1]
  true.positives / (true.positives + false.positives)
}

recall = function(predicted, actual) {
  t = table(predicted, actual)
  true.negatives = t[1, 1]
  true.positives = t[2, 2]
  false.negatives = t[1, 2]
  false.positives = t[2, 1]
  true.positives / (true.positives + false.negatives)
}

f.score = function(predicted, actual) {
  p = precision(predicted, expected)
  r = recall(predicted, expected)
  2 * p * r / (p + r)
}

# learning
# install.packages('e1071', dependencies = TRUE)

library(class)
library(e1071) 

if (!require(tree)) {
  install.packages("tree")
  library(tree)
}

# for calculating scores
expected = test[, 1]
test_input = test[2:length(test)]



summary(train)


#
# prediction models
#

# Trees

tree1 = tree(survived ~ pclass + sex + age + sibsp + parch + fare + embarked, data=train)
summary(tree1)
summary(tree1)$used

# only sex + pclass + fare + age + sibsp are used, according to the summary
tree1 = tree(survived ~ sex + pclass + fare + age + sibsp, data=train)
summary(tree1)

plot(tree1, col="blue", type="uniform")
text(tree1, cex=.5, col="red")
summary(pruned)$used

prediction.tree1 <- predict(tree1, test_input)
prediction.tree1 = prediction.tree1 > 0.5

table(prediction.tree1, expected)

precision(prediction.tree1, expected)
recall(prediction.tree1, expected)
f.score(prediction.tree1, expected)

# lets prune the tree
pruned <- prune.tree(tree1, best=7)
plot(pruned, col="blue", type="uniform")
text(pruned, cex=.5, col="red")

prediction.pruned <- predict(pruned, test_input)
prediction.pruned = prediction.pruned > 0.5

table(prediction.pruned, expected)
precision(prediction.pruned, expected)
recall(prediction.pruned, expected)
f.score(prediction.pruned, expected)


# random forests
library(randomForest)
set.seed(4123)

rnd.forest = randomForest(survived ~ pclass + sex + age + sibsp + parch + 
                          fare + embarked, 
                          data=train, 
                          importance=T,
                          ntree=5000)

# importance of each variable
importance(rnd.forest)

prediction.rnd.forest = predict(rnd.forest, test_input)
prediction.rnd.forest = prediction.rnd.forest > 0.5

table(prediction.rnd.forest, expected)

precision(prediction.rnd.forest, expected)
recall(prediction.rnd.forest, expected)
f.score(prediction.rnd.forest, expected)



# NN
# install.packages("nnet")
library(nnet)

nnet1 = nnet(survived ~ sex + pclass + fare + age + sibsp, 
             data=train, size = 2, rang = 0.1,
             decay = 5e-4, maxit = 200)

prediction.nn = predict(nnet1, test_input)
prediction.nn = prediction.nn > 0.5

table(prediction.nn, expected)

precision(prediction.nn, expected)
recall(prediction.nn, expected)
f.score(prediction.nn, expected)


# TEST
test = read.csv("test.csv")

# getting rid of NAs in the test data
which(!complete.cases(test))
test$age[is.na(test$age)] = mean.age


test[which(!complete.cases(test)),]

mean.fare = mean(c(train$fare, na.omit(test$fare)))
test$fare[is.na(test$fare)] = mean.fare

which(!complete.cases(test))

# pclass as a factor
test$pclass = as.factor(test$pclass)

# checking that test and train data are of the same classes
sapply(test[1, ], class)
sapply(test_input[1, ], class)
which(sapply(test[1, ], class) != sapply(test_input[1, ], class))
all(sapply(test[1, ], class) == sapply(test_input[1, ], class))


# == tree ==
prediction.tree1 <- predict(tree1, test)
prediction.tree1 = as.integer(prediction.tree1 > 0.5)

result = data.frame(survived=prediction.tree1)
write.csv(result, "01-tree1.csv", row.names=F)


# == pruned tree ==
prediction.pruned <- predict(pruned, test)
prediction.pruned = as.integer(prediction.pruned > 0.5)

result = data.frame(survived=prediction.pruned)
write.csv(result, "02-pruned.csv", row.names=F)


# == random forest ==
# add missing level
levels(train$embarked)
levels(test$embarked)

test$embarked = factor(test$embarked, levels=levels(train$embarked))
levels(test$embarked)

prediction.rnd.forest = predict(rnd.forest, test)
prediction.rnd.forest = as.integer(prediction.rnd.forest > 0.5)

result = data.frame(survived=prediction.rnd.forest)
write.csv(result, "03-random-forest.csv", row.names=F)

# == neural network ==
prediction.nn = predict(nnet1, test)
prediction.nn = as.integer(prediction.nn > 0.5)

result = data.frame(survived=prediction.nn)
write.csv(result, "04-neural-network.csv", row.names=F)
