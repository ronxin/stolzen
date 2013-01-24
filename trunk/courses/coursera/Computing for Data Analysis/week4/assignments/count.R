count <- function(cause = NULL) {
  ## Check that "cause" is non-NULL; else throw error
  if (is.null(cause)) {
    stop("cause is NULL")
  }
  
  ## Read "homicides.txt" data file
  homicides <- readLines("homicides.txt")

  ## Extract causes of death
  pattern = paste("<dd>cause: ", cause, "</dd>", sep="")
  res = grep(pattern, homicides,ignore.case = TRUE)

  ## Return integer containing count of homicides for that cause
  res = length(res)
  
  ## Check that specific "cause" is allowed; else throw error
  if (res == 0) {
    stop("non-existing cause")
  }

  res
}