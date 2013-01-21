outcome.data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

outcome.data[, 11] <- as.numeric(outcome.data[, 11]) 
outcome.data[, 17] <- as.numeric(outcome.data[, 17]) 
outcome.data[, 23] <- as.numeric(outcome.data[, 23]) 

outcome.names = list("heart attack"=11, "heart failure"=17, "pneumonia"=23)

all.states = levels(factor(outcome$State))

rankhospital = function(state, outcome.name, num="best") {
  col = outcome.names[[outcome.name]]
  
  if (is.null(col)) {
    stop("invalid outcome")
  }
  
  if (!(state %in% all.states)) {
    stop("invalid state")
  }
  
  state.rows = outcome$State == state
  hospitals = outcome$Hospital.Name[state.rows]
  deaths = outcome[state.rows, col]
  
  # na.last = NA means to omit NAs altogether
  o = order(deaths, hospitals, na.last=NA)
  
  index = if (num == "best") {
    1
  } else if (num == "worst") {
    length(o)
  } else {
    num
  }

  # result
  hospitals[o[index]]
}