corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  c = complete(directory)
  ids = c[c$nobs > threshold, ]$id
  
  calculate.cor = function(row) {
    cc = complete.cases(row)
    cor(row[cc, ]$sulfate, row[cc, ]$nitrate)
  }
  
  above.threshold = lapply(ids, getmonitor, directory = directory)
  sapply(above.threshold, calculate.cor)
}