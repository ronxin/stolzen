source('rankhospital.R')

outcome.names = list("heart attack"=11, "heart failure"=17, "pneumonia"=23)
all.states = levels(factor(outcome$State))


rankall <- function(outcome, num = "best") {
  ## Read outcome data
  
  col = outcome.names[[outcome]]
  
  if (is.null(col)) {
    stop("invalid outcome")
  }
  
  best.h = sapply(all.states, rankhospital, outcome.name=outcome, num=num)
  data.frame(hospital=best.h, state=all.states)
}

