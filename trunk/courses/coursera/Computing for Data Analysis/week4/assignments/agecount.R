agecount <- function(age = NULL) {
  ## Check that "age" is non-NULL; else throw error
  ## Check that "cause" is non-NULL; else throw error
  if (is.null(age)) {
    stop("age is NULL")
  }
  
  ## Read "homicides.txt" data file
  homicides <- readLines("homicides.txt")
  
  ## Extract ages of victims; ignore records where no age is given
  pattern = sprintf("\\s+%d\\s+years\\s+old", age)
  res = grep(pattern, homicides,ignore.case = TRUE)
  
  ## Return integer containing count of homicides for that age
  length(res)
}