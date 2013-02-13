download.file(url="https://spark-public.s3.amazonaws.com/dataanalysis/movies.txt", 
              destfile="movies.txt")

movies <- read.delim("movies.txt", sep="\t", header=TRUE)

sapply(movies[1,], class)

# 1
# Fit a linear regression model by least squares where 
# the Rotten Tomatoes score is the outcome 
# the box office gross is the only covariate. 
# What is the regression coefficient for the slope and it's interpretation?

lm.score.vs.box.office = lm(movies$score ~ movies$box.office)

plot(movies$score ~ movies$box.office, col="blue", pch=19)
abline(lm.score.vs.box.office)

lm.score.vs.box.office

# The regression coefficient is 0.09676. 
# The interpretation is that an increase of one million dollars in box office gross is 
# associated with a 0.09676 increase in Rotten Tomatoes Score.

# 2
# Fit a linear regression model by least squares where 
# the Rotten Tomatoes score is the outcome 
# the box office gross is the only covariate. 
# What is the 90% confidence interval for the intercept term and what can you 
# deduce from the 90% confidence interval?

summary(lm.score.vs.box.office)
confint(lm.score.vs.box.office)

# 90% confidence
confint(lm.score.vs.box.office, level=0.9)

# The 90% confidence interval for the intercept is (47.52, 52.63). 
# If we repeated this study 100 times, we would expect our calculated 
# interval to cover the true value on average 90% of the time.


# 3
# Fit a linear regression model by least squares where 
# the Rotten Tomatoes score is the outcome 
# box office gross and running time are the covariates. 
# What is the value for the regression coefficient for running time? 
# How is it interpreted?

lm3 = lm(movies$score ~ movies$box.office + movies$running.time)

lm3


# The coefficient is 0.12752. 
# That means that an increase of one minute in running time is associated with 
# an average increase of 0.12752 in score.

# 4

plot(movies$score ~ movies$box.office, col="blue", pch=19)
abline(lm.score.vs.box.office)
abline(lm3)

# No running time is not a confounder.  
# Including it in the regression model does not change the direction (sign)  
# of the relationship between box office gross and Rotten Tomatoes score.

cor(movies$running.time, movies$score)
cor(movies$running.time, movies$box.office)


# 5

plot(movies$score ~ movies$running.time, col="blue", pch=19)
# note 2 outliers

which(movies$running.time >= 200)
movies.good = movies[movies$running.time < 200, ]
plot(movies.good$score ~ movies.good$running.time, col="blue", pch=19)

summary(lm3)

lm5 = lm(movies.good$score ~ movies.good$box.office + movies.good$running.time)
summary(lm5)

# old 0.12752 < new  0.21879
# 0.0187 < 0.001191 - more statically significant


# 6

summary(lm3)


# 7
# Fit a linear model by least squares where 
# Rotten Tomatoes score is the outcome 
# the covariates are movie rating, running time, 
# and an interaction between running time and rating are the covariates. 
# What is the coefficient for the interaction between running time and 
# the indicator/dummy variable for PG rating?
lm7 = lm(movies$score ~ movies$box.office + movies$running.time + 
           movies$running.time * movies$rating)

summary(lm7)


# 8 
# also something weird 


# 9 
# Fit a linear model where 
# - the outcome is the number of breaks  
# - the covariate is tension.  
# What is a 95% confidence interval for the average difference in number of breaks  
# between medium and high tension?

data(warpbreaks)

lm9 <- aov(warpbreaks$breaks ~ as.factor(warpbreaks$tension))
TukeyHSD(lm9)
