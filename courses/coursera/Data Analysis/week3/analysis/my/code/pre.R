default.par = par()
setwd("C:/Users/Alexey Grigorev/Documents/courses/coursera/Data Analysis/week3/analysis/my")

loans = read.csv("data/loansData.csv")

summary(loans)

sapply(loans[1,], class)
# note that Interest.Rate abd Debt.To.Income.Ration are characters
# so let's transform them into numeric

loans$Interest.Rate = as.numeric(sub(pattern="%", replacement="", 
                                     loans$Interest.Rate)) / 100

loans$Debt.To.Income.Ratio = as.numeric(sub(pattern="%", replacement="", 
                                     loans$Debt.To.Income.Ratio)) / 100


# also there are some NAs, lets remove them
loans = loans[complete.cases(loans), ]



# 2 observations with NA gone

attach(loans)


#
# Explanatiory analysis
#

hist(Interest.Rate, breaks=50)
quantile(Interest.Rate)

# Amount.Requested
hist(Amount.Requested)
quantile(Amount.Requested)

# let's play and see what may affect Interest.Rate

plot(Interest.Rate ~ Amount.Requested)
# no visible pattern

plot(Interest.Rate ~ Amount.Requested, col=rgb(0, 0, 1, 0.1), pch=19)
smoothScatter(Interest.Rate ~ Amount.Requested)
# there seems to be several densest spots

# try to use the same, but on groups

cor(Interest.Rate, Amount.Requested)
# and correlation is not very high


Amount.Requested.Cut = cut(Amount.Requested, breaks=5)
# giving meaningful names to levels
levels(Amount.Requested.Cut) = 
  as.character(tapply(Amount.Requested, Amount.Requested.Cut, median))
#  paste(
#    as.character(tapply(Amount.Requested, Amount.Requested.Cut, min)),
#    as.character(tapply(Amount.Requested, Amount.Requested.Cut, max)),
#  )
    


boxplot(Interest.Rate ~ Amount.Requested.Cut, varwidth=TRUE) #, pars=list(las=2))
# but if split, it's apparent that the bigger requested amount, 
# the higher is the interest rate


# Loan.Length
boxplot(Interest.Rate ~ Loan.Length, varwidth=TRUE)
# for longet loans interest rate tend to be higher
# apparent relation

boxplot(Amount.Requested ~ Loan.Length)


# Loan Purpose
par(mar=c(9, 3, 1, 1))
boxplot(Interest.Rate ~ Loan.Purpose, pars=list(las=2), varwidth=TRUE)

# there seems to be a pattern
# lets sort by medians

sort.by.medians = function(what, fac) {
  # from computing for data analysis, week 3 assigment 3

  medians = tapply(what, fac, median)
  # order level names of the factor
  ordered.levels = levels(fac)[order(medians)]
  # and order the factor by ordered names
  ordered.by.medians = ordered(fac, ordered.levels)
  ordered.by.medians
}


boxplot(Interest.Rate ~ sort.by.medians(Interest.Rate, Loan.Purpose), 
        pars=list(las=2), varwidth=TRUE)
par(mar=default.par$mar)

# so renewable evergy, education and car are with the lowest interest rate
# dept consolidation is with the highest

boxplot(Interest.Rate ~ State)
boxplot(Interest.Rate ~ sort.by.medians(Interest.Rate, State),
        pars=list(las=2))
# also in some states median of interest rate is lower than in others

# Debt.To.Income.Ratio
plot(Interest.Rate ~ Debt.To.Income.Ratio)
smoothScatter(Interest.Rate ~ Debt.To.Income.Ratio)
cor(Interest.Rate, Debt.To.Income.Ratio)
# no visible pattern and correlation is very low

hist(Debt.To.Income.Ratio)
# seems like a normal distribution

# lets try cutting it and see the interest rate
Debt.To.Income.Ratio.Cut = cut(Debt.To.Income.Ratio, breaks=9)
levels(Debt.To.Income.Ratio.Cut) = 
  as.character(tapply(Debt.To.Income.Ratio, Debt.To.Income.Ratio.Cut, median))

boxplot(Interest.Rate ~ Debt.To.Income.Ratio.Cut, 
        pars=list(las=2), varwidth=TRUE)
# now without ordering it's obvious that the higher the debt/income ration
# the higher interest rate


# Home.Ownership
boxplot(Interest.Rate ~ Home.Ownership)
# doesn't seem to be relevant at all


# Monthly.Income
plot(Interest.Rate ~ Monthly.Income)
plot(Interest.Rate ~ log10(1 + Monthly.Income), col=rgb(0, 0, 1, 0.2), pch=19)
# no visible pattern

smoothScatter(Interest.Rate ~ log10(1 + Monthly.Income))
# it seems that it does depend in many cases, but the veriance is too high

cor(Interest.Rate, Monthly.Income)
# no correlation

boxplot(Interest.Rate ~ cut(log10(1 + Monthly.Income), breaks=15))
# there seems to be no pattern


# Open.CREDIT.Lines
boxplot(Interest.Rate ~ Open.CREDIT.Lines)
# for small values it seems to make no difference,
# however, for biffer IR does get bigger

hist(Open.CREDIT.Lines)

Open.CREDIT.Lines.Cut = cut(Open.CREDIT.Lines, breaks=15)
boxplot(Interest.Rate ~ Open.CREDIT.Lines.Cut)

Open.CREDIT.Lines.CutL = cut(Open.CREDIT.Lines, breaks=7)
boxplot(Interest.Rate ~ Open.CREDIT.Lines.CutL, varwidth=TRUE)
# so it indeed does (we can omit last 2 values as they are not frequent)


# Revolving.CREDIT.Balance
smoothScatter(Interest.Rate ~ log10(1 + Revolving.CREDIT.Balance))
cor(Interest.Rate, Revolving.CREDIT.Balance)
# no correlation

boxplot(Interest.Rate ~ cut(Revolving.CREDIT.Balance, breaks=10))
boxplot(Interest.Rate ~ cut(log10(1 + Revolving.CREDIT.Balance), breaks=10))
# doesn't seem to have a pattern


# Inquiries.in.the.Last.6.Months
table(Inquiries.in.the.Last.6.Months)

boxplot(Interest.Rate ~ Inquiries.in.the.Last.6.Months, varwidth=TRUE)
# probably there is a trend:
# 0 < 1 < 2 < 3 
# why 4, 5 and 7 are lower? 
# (seems the reason is that they are rare)


# Employment.Length
boxplot(Interest.Rate ~ Employment.Length)
# seems no pattern


# FICO range
boxplot(Interest.Rate ~ FICO.Range)
# obviously there is a pattern 
# the higher FICO score is, the less interest rate is


# so the following variables seem to have influence on Interest.Rate:
# Amount.Requested
# Loan.Length
# Loan.Purpose
# State # seems needn't take into accont
# Debt.To.Income.Ratio
# Open.CREDIT.Lines # trend is weak

detach(loans)


# let's concentrate on variables that seem to matter
loans1 = loans[, c("Interest.Rate", "Amount.Requested", "Loan.Length", 
          "Loan.Purpose", "Debt.To.Income.Ratio")]

attach(loans1)

lm1 = lm(Interest.Rate ~ Amount.Requested)
plot(Interest.Rate ~ jitter(Amount.Requested), col="blue", pch=19, cex=0.3)
abline(lm1)

plot(Interest.Rate, lm1$residuals, col = Loan.Length, pch = 19, cex=0.7)
# appears to be non-random pattern here
plot(Interest.Rate, lm1$residuals, col = Loan.Purpose, pch = 19, cex=0.7)
# harder to say if there is a pattern


# Now fit a model with factor adjustment for loan purpuse, loan length, 
# and debt-to-income raion


lm2 = lm(Interest.Rate ~ Amount.Requested + Loan.Length + 
           Loan.Purpose + Debt.To.Income.Ratio)


plot(Interest.Rate, lm2$residuals, col = Loan.Length, pch = 19, cex=0.7)
# far better!
plot(Interest.Rate, lm2$residuals, col = Loan.Purpose, pch = 19, cex=0.5)
plot(Interest.Rate, lm2$residuals, col = Debt.To.Income.Ratio.Cut, pch = 19, 
     cex=0.5)
# also some patterns

# estimates and confidence intervals
summary(lm2)

# correlation:
sqrt(summary(lm2)$r.square)

confint(lm2)


# try to see what will happen if we apply cluster analysis
d = dist(loans1)
h = hclust(d)

myplclust <- function(hclust,lab = hclust$labels, lab.col = rep(1, length(hclust$labels)),
                      hang=0.1, ...) { 
  y <- rep(hclust$height, 2)
  x <- as.numeric(hclust$merge)
  
  y <- y[which(x<0)]
  x <- x[which(x<0)]
  x <- abs(x)
  y <- y[order(x)]
  x <- x[order(x)]
  
  plot(hclust, labels=FALSE, hang=hang, ...)
  
  text(x=x, y=y[hclust$order]-(max(hclust$height)*hang),
       labels=lab[hclust$order], col=lab.col[hclust$order], 
       srt=90, adj=c(1, 0.5), xpd=NA, ...)
}


myplclust(h, lab.col=as.numeric(loans1$Loan.Purpose))
myplclust(h, lab.col=as.numeric(loans1$Loan.Length))


plot(h)

# rect.hclust(h, h=10000, border="red")
# rect.hclust(h, h=15000, border="red")
# rect.hclust(h, h=9000, border="red")
# rect.hclust(h, h=8000, border="red")
rect.hclust(h, h=6000, border="red")

clusters = cutree(h, h=6000)
unique(clusters)


boxplot(Interest.Rate ~ clusters)
boxplot(Amount.Requested ~ clusters)
boxplot(Debt.To.Income.Ratio ~ clusters)

plot(Interest.Rate ~ Amount.Requested, col=clusters)
# need to scale data



detach(loans1)

loans2 = loans1

loans2$Amount.Requested = scale(loans2$Amount.Requested)
loans2$Interest.Rate = scale(loans2$Interest.Rate)
loans2$Debt.To.Income.Ratio = scale(loans2$Debt.To.Income.Ratio)

d = dist(loans2)
h = hclust(d)

plot(h)

rect.hclust(h, h=5, border="red")

clusters = cutree(h, h=5)
table(clusters)

attach(loans2)

boxplot(Interest.Rate ~ clusters)
# lets order clustes by medians of interest rate
sorted.clusters = sort.by.medians(Interest.Rate, as.factor(clusters))
sorted.clusters = as.numeric(sorted.clusters)

boxplot(Interest.Rate ~ sorted.clusters)

boxplot(Amount.Requested ~ sorted.clusters)
boxplot(Debt.To.Income.Ratio ~ sorted.clusters)

plot(Interest.Rate ~ Amount.Requested, col=clusters, pch=19)

detach(loans2)


clusters = sorted.clusters
table(clusters)


plot(loans1$Interest.Rate ~ loans1$Amount.Requested, col=clusters, pch=19)
plot(loans1$Interest.Rate ~ loans1$Debt.To.Income.Ratio, col=clusters, pch=19)
plot(loans1$Amount.Requested ~ loans1$Debt.To.Income.Ratio, col=clusters, pch=19)

# let's try using only quantatitive (scaled) variables

loans3 = loans2[, c("Interest.Rate", "Amount.Requested", "Debt.To.Income.Ratio")]

svd1 = svd(loans3)

plot(svd1$u[,1], col=clusters, pch=19)
plot(svd1$u[,2], col=clusters, pch=19)

plot(svd1$u, col=clusters, pch=19)

# maximal contributor
plot(svd1$v[,2], pch=19)


FICO.Range.min = as.numeric(sapply(strsplit(as.character(loans$FICO.Range), "-"), function(x) x[1]))

lm.fico = lm(Interest.Rate ~ FICO.Range.min)
summary(lm.fico)
sqrt(summary(lm.fico)$r.squared)
