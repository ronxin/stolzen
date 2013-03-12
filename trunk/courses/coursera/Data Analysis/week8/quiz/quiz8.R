# 1

V = 5
m0 = 55
V / m0

R = 25
V / R


# 2
set.seed(3343)
pValues = rep(NA,100)
for(i in 1:100){
  z = rnorm(20)
  x = rnorm(20)
  y = rnorm(20,mean=0.5*x)
  pValues[i] = summary(lm(y ~ x))$coef[2,4]
}

alpha = 0.1

# Controls FWER
sum(p.adjust(pValues, method="bonferroni") < alpha)

# Controls FDR
sum(p.adjust(pValues, method="BH") < alpha)
