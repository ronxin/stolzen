par(mfrow = c(3, 1))

range.without.na = function(col.number) {
  range(na.omit(outcome[, col.number]))
}

minmax = c(range.without.na(11), range.without.na(17), range.without.na(23))))
xlim = c(min(minmax), max(minmax))

draw.hist = function(col.number, name) {
  meanval = mean(na.omit(outcome[, col.number]))
  hist(outcome[, col.number], 
       main=substitute(name * " (" * bar(X) == m * ")", list(name=name, m=meanval)),
       xlab="30-day Death Rate", 
       xlim=xlim)
  abline(v = median(na.omit(outcome[, col.number])))
}

draw.hist(11, "Heart Attack")
draw.hist(17, "Heart Failure")
draw.hist(23, "Pneumonia")