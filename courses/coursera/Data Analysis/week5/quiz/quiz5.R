data(warpbreaks)
head(warpbreaks)
aov1 = aov(warpbreaks$breaks ~ warpbreaks$wool + warpbreaks$tension)
aov1
anova(aov1)


install.packages("glm2")
library(glm2)
data(crabs)
head(crabs)
glm1 = glm(crabs$Satellites ~ crabs$Width)
glm1
glm1$coeff
exp(glm1$coeff)
glm1$coeff

plot(crabs$Width ~ crabs$Satellites)
glm1 = glm(crabs$Satellites ~ crabs$Width, family="poisson")

b1 = glm1$coeff[2]
b0 = glm1$coeff[1]
exp(b0) * exp(b1 * 22)


library(MASS)
data(quine)
lm1 = lm(log(Days + 2.5) ~.,data=quine)

s = step(lm1)
s
