
load("data/samsungData.rda")

names(samsungData) 
length(names(samsungData))

table(complete.cases(samsungData))


length(unique(names(samsungData)))
unique.names = unique(names(samsungData))


unique(samsungData$activity)

samsungData$activity = as.factor(samsungData$activity)

samsungData = subset(samsungData, select=unique.names)

# fix the column names
names1 = gsub(pattern="-", replacement=".", names(samsungData))
names1 = gsub(pattern=",", replacement="", names1)
names1 = gsub(pattern="[()]", replacement="", names1)
names(samsungData) = names1

subjects = unique(samsungData$subject)
length(subjects)

subjectsTraining = subjects[1:(length(subjects)/2 + 4)]
subjectsTraining

samsungTraining = samsungData[samsungData$subject %in% subjectsTraining, ]
# remove subject column
samsungTraining = subset(samsungTraining, select=-c(subject))

samsungTesting = samsungData[!(samsungData$subject %in% subjectsTraining), ]

# naive bayer classifier
# http://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Classification/Na%C3%AFve_Bayes
install.packages('e1071', dependencies = TRUE)

library(class) 
library(e1071) 

dim(samsungTraining)

bayes = naiveBayes(activity ~ ., data=samsungTraining)
prediction.bayes = predict(bayes, samsungTesting[, 1:477])

table(prediction.bayes, samsungTesting[, 479])


# svm
# http://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Classification/SVM
svm1 = svm(activity ~ ., data=samsungTraining)
summary(svm1)

dim(samsungTesting)

prediction.svm = predict(svm1, samsungTesting[, 1:477])
table(prediction.svm, samsungTesting[, 479])



# trees
if (!require(tree)) {
  install.packages("tree")
  library(tree)
}

tree1 = tree(activity ~ ., data=samsungTraining)
summary(tree1)


plot(tree1,col="blue",type="uniform")
text(tree1,cex=.5,col="red")

par(mfrow=c(1,1))

prediction.tree1 <- predict(tree1, samsungTesting[, 1:477], type="class")
table(prediction.tree1, samsungTesting[, 479])

pruned <- prune.tree(tree1, best=6)




names(pruned)

used.variables = summary(pruned)$used
used.variables
plot(pruned, col="blue", type="uniform")
text(pruned, cex=.5, col="black")

prediction.tree2 <- predict(pruned, samsungTesting[, 1:477], type="class")
table(prediction.tree2, samsungTesting[, 479])

# random forsts

library(randomForest)

rnd.forest = randomForest(activity ~ ., data=samsungTraining, ntree=15) 
prediction.rnd.forest = predict(rnd.forest, samsungTesting[, 1:477])

table(prediction.rnd.forest, samsungTesting[, 479])

# neural networks

# install.packages("nnet")
# library(nnet)
# 
# dim(samsungTraining)
# nnet1 = nnet(activity ~ ., data=samsungTraining, size = 2, rang = 0.1,
#              decay = 5e-4, maxit = 200)
# 
# prediction.nn = predict(nnet1, samsungTesting[, 1:477], type="class")
# table(prediction.nn, samsungTesting[, 479])

# not good

# combinig predictors

prediction.df = cbind(as.character(prediction.svm),
                      as.character(prediction.rnd.forest))


majority.vote = function(line) {
  t = table(line)
  names(which.max(t))
}

best = as.factor(apply(prediction.df, 1, majority.vote))


error = function(our, test) {
  good = length(which(our != test))
  good / length(our)
}

actual = as.factor(samsungTesting[, 479])


error(prediction.svm, actual)
error(prediction.rnd.forest, actual)

table(best, actual)
error(best, actual)

# best is SVM



install.packages("RColorBrewer")
library(RColorBrewer)


## Set up a function that makes colors prettier
mypar <- function(a = 1, b = 1, brewer.n = 8, brewer.name = "Dark2", ...) {
  par(mar = c(2.5, 2.5, 1.6, 1.1), mgp = c(1.5, 0.5, 0))
  par(mfrow = c(a, b), ...)
  palette(brewer.pal(brewer.n, brewer.name))
}

## Set size of axes
cx = 1.3

mypar(mfrow = c(1, 3))


plot(pruned, col="blue", type="uniform", main="(a)")
text(pruned, cex=0.6, col="black", main="(a)")

plot(tGravityAcc.max.Y ~ fBodyAccJerk.std.X, data=samsungTesting, col=actual, pch=19, 
     ylim=c(-0.3, 0.0),
     xlab="body jerk",
     ylab="gravity acceleration",
     main="(b)")

plot(tGravityAcc.max.Y ~ tGravityAcc.min.X, data=samsungTesting, col=actual, pch=19,
     xlim=c(0.75, 1),
     ylim=c(-0.3, 0.2),
     ylab="gravity acceleration y",
     xlab="gravity acceleration x",
     main="(c)")


dev.copy2pdf(file="finalfigure.pdf", height = 4, width = 3 * 4, out.type="pdf")
