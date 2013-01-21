source("rankhospital.R")

best = function(state, outcome.name) {
  if (is.null(col)) {
    stop("invalid outcome")
  }
  
  if (!(state %in% all.states)) {
    stop("invalid state")
  }
  
  rankhospital(state, outcome.name, "best")
}