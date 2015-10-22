Topic: Computing for Data Analysis




# Week1 #

## Syntax ##
  * x <- assigment
  * `#` comment
  * x <- 1:20 - sequence

## Data types ##
### atomic classes ###
  * logic
  * characters
  * numeric
  * integer
  * complex
### numbers ###
  * doubles by default
  * if integer is needed - use L
  * Inf = 1/ 0
  * -Inf
  * NaN - not a number
### vector ###
  * everything of the same class
  * creating a vector
    * vector() creates a new vector
    * x <- vector("numeric", length = 10)
      * creates a vector filled with NA
    * c() concatenate - other function for creating vector
    * c(0.5, 0.6) numeric
    * c(TRUE, FALSE); c(T, F) logical
    * c("a", "b") character
    * 9:29 integer
    * c(1+0i, 2+4i) complex
### matrix ###
  * vector with dimension attributes
    * nrow
    * ncol
  * creating
    * m <- matrix(nrow = 2, ncol = 3)
    * m <- matrix(1:6, nrow = 2, ncol = 3)
      * from upper left down
      * 1 3 5
2 4 6
    * column-binding
      * x <- 1:3; y <- 10:12
      * cbind(x, y)
        * 1 10
2 11
3 12
      * rbind(x, y)
        * 1   2   3
10 11 12
### list ###
  * may contain different types
  * x <- list(1, "a", T, 1+4i)
### attributes ###
  * names (dimnames)
  * dimensions
  * class
  * length
  * other user defined data
  * attributes() - general function
### factor ###
  * categorical data vector
    * ordered
    * unordered
  * integer vector with each integer having a label
  * self-describing
  * x <- factor(c("yes", "yes", "no", "yes", "no")
  * table(x) => frequency
  * unclass(x)
    * 2 2 1 2 2 1 `#`(2=yes, 1=no)
  * gl - generate factors
    * f = gl(3, 10) `#` 1..1 x10, 2..2 x10, 3..3 x10
### data frame ###
  * for storing tabular data
  * special type of list
  * special attribute: row.names
  * read.table() read.csv() return data frame
  * can be converted to matrix
    * but it will be coerced
  * x <- data.frame(foo = 1:4, bar = c(T, T, F, F))
  * nrow(x)
  * ncol(x)
### convertion ###
  * coercion
    * finds "last common denom"
    * y <- c(1.7, "a"): 1.7 -> "1.7", "a" (character)
    * c(T, 2) -> 1, 2 (TRUE becomes 1)
    * c("a", TRUE) -> "a", "TRUE" (T becomes "TRUE")
  * explicit
    * as.`*` functions
    * as.numeric()
    * as.logical()
    * as.character()
    * as.complex()
    * if no convertion possible, returns NA
### missing values ###
  * NaN
  * NA
  * is.na()
    * is.na(NA) => T
    * is.na(NaN) => T
  * is.nan()
    * is.nan(NA) => F
    * is.nan(NaN) = T
  * is.na(c(1, NA, 2)) => F T F
### naming ###
  * if named, will be printed with names
  * vectors
    * x <- 1:3
names(x) => NULL
names(x) <- c("foo", "bar", "n")
names(x) => given names
  * lists
    * x <- list(a=1, b=2, c=3)
  * matrices
    * m <- matrix(1:4, nrow=2, ncol=2)
dimnames(m) <- list(c("a", "b"), c("c", "d"))
    * m =>
> > > c  d
a  1  3
b  2  4

## Read/Write ##
### tabular ###
  * read
    * read.table
      * arguments:
        * file`*`
        * header
          * logical - variable names in the first row?
        * sep
          * separator ("," etc)
        * colClasses
          * types of classes in each column
          * is not specified, R figures out the types
        * nrows
          * number of rows to read
          * default is all the file
        * comment
          * char, after which nothing is read
          * default is `#`
        * skip
          * number of lines to skip
      * data <- read.table("foo.txt")
    * read.csv
      * same as read.table, but default separator is ","
    * for large files
      * RTFM!
      * make a rough calculation of RAM needed
        * 1.5 mln rows `*` 120 cols of numeric data
        * 1.5 mln `*` 120 `*` 8 bytes = 1.34 GB
        * consider overhead! 1.43 `*` 2 ~ 2.7 GB
      * comment.char = "" - faster
      * colClasses set explicitly - much faster
        * initial <- read.table("db.txt", nrows=100)
classes <- sapply(initial, class) `#`automatically figured out classes
head(initial) `#`just shows the fist 6 lines
tabAll <- read.table("db.txt", colClasses=classes)
  * write
    * write.table
    * write.csv
### R objects ###
  * metadata, as R source
    * editable!
    * good for VCS
    * but not very space efficient
  * read
    * source
    * dget
  * write
    * dump
    * dput
  * `#` for one variable
y <- data.frame(a=1, b="a")
dput(y) => to the console
dput(y, file="y.R")
new.y <- dget("y.R")
  * `#` for several variables
x <- smth, y <- smth
dump(c("x", "y"), file="data.R")
rm(x, y) `#` removes x y
source("data.R")
  * Subtopic 5
### character ###
  * readLines
  * writeLines
### serialize/unserialize ###
### File connections ###
  * file
    * plain file
  * gzfile
    * gzip
  * bzfile
    * bzip2
  * url
    * to a web page
  * con <- file("foo.txt", "r")
data <- read.csv(con)
close(con)
`#` the same as read.csv("foo.txt")
  * con <- gzfile("w.gz")
x <- readLines(con, 10) `#`first 10 lines
  * con <- url("http://...", "r")
x <- readLines(con)

## Accessing subsets ##
### [ ###
  * returns the same type
  * starts from 1
### [[ ###
  * for lists and data frames
### $ ###
  * from lists and data frames, but using name
### vectors ###
  * x <- c("a", "b", "c", "c", "d", "a")
x[1](1.md) => a
x[1:4] => a b c d
x[> "a"](x.md) => b c c d
`#` the same:
u <- x > "a" => F T T T T F
x[u](u.md) => b c c d
`#`only trues
### matrix ###
  * m[row, col]
  * x <- matrix(1:6, 2, 3)
x[1, 2] => 3
x[2, 1] => 2
  * m[1, ] => first row
  * m[, 2] => secod col
  * return VECTOR!
    * x[1, 2, drop=F] => matrix of one element
    * x[1, ,drop=F] => a matrix of one row
### lists ###
  * x <- list(foo=1:4, bar=0.6)
  * x[1](1.md) => sublist
$foo
[1](1.md) 1 2 3 4
  * x[[1](1.md)] => (element) 1 2 3 4
  * x$bar <=> x[["bar"]] => 0.6
  * x["bar"] => sublist
$bar
[1](1.md) 0.6
  * x[c(1, 3)] returns several cols
  * name <- "bar"
x[name](name.md) => smth
x$name => NULL! ("name" doesn't exist)
  * x[[c(1, 3)]] => 1st list, 3th el
    * the same: x[[1](1.md)][[3](3.md)]
  * partial matching
    * x <- list(longname=1:5)
x$lo => returns $longname
x[["lo"]] => NULL
x[["lo", exact=F]] => $longname
### removing NAs ###
  * bad <- is.na(x)
x[!bad] `#`note the inversion!
  * good <- complete.cases(x, y)
x[good](good.md); y[good](good.md)
  * r <- matrix
good <- complete.cases(r)
r[good, ]
### vectorized operations ###
  * vector
    * no need for looping!
    * x <- 1:4; y <- 6:9
    * x + y
    * x > 2
    * x >= 2
    * y == 8
    * x `*` y
    * x / y
  * matrix
    * x <- matrix(1:4, 2, 2)
y <- matrix(rep(10, 4)), 2, 2)
    * x `*` y => element-wise
    * x / y => element-wise
    * x %`*`% y - true matrix multiplication


# Week2 #

## Control Structures ##
### conditions ###
  * if (condition) {

> `#` smth
} else if (cond2) {
> `#` smth
} else {
> `#` smth
}
### blocks return result ###
  * y = if (x > 3) {
  1. 
} else {
> 0
}
### loops ###
  * while
    * while (condition) {
> `#`do smth
}
  * repeat
    * infinite loop
    * repeat {
> `#` smth
> if (smth) {
> > break;

> }
}
  * for
    * for (i in 1:10) { print(i) }
    * for (i in seq`_`lenght(nrow(x))) {
> print(x[i](i.md))
}
  * next
    * like continue
  * break

## Functions ##
### high-level objects ###
  * may take function as parameters, or return functions
### declaration ###
  * f = function(arg.x, arg.y, def.val = NULL) {
> 4 `#` no need for return
}
  * f = function(arg.x, ...) {
> `#` ... - any other arguments
> another.function(10, 20, ...)
}
### scoping ###
  * lexical scoping
  * environments
    * env - name-value pairs
    * env can have a parent and multiple children
    * env of a function - its closure
    * global env
      * seach() - list of packages
    * local function environment
      * consider make.power function
        * make.power = function(n) {
> pow = function(x) {
> > x ^ n

> }
}
    * cube = make.power(3)
    * ls(environment(cube)) `#`lists variables
    * get("n", environment(cube)) `#`returns variable n
  * look up
    * local env
    * if not in the function env, look up in the parental
    * until the top one reached
    * global
### args(f) returns list of arguments ###

## Loop function ##
### lapply ###
  * lapply(X, FUN, ...)
  * X -list, returns a list
  * if X not a list, it will be coerced
  * x = list(a = 1:5, b=rnorm(10))
lapply(x, mean)
### sapply ###
  * same as lapply, but simplifies the result
  * if each el contains one element, result coerced to vector
  * if each el of the same len, result coerced to matrix
  * otherwise a list is returned
### apply ###
  * evaluates the function over the margin of array
  * apply(X, MARGIN, FUN, ...)
  * MARGIN: 1 for row, 2 for col
    * rowSum = apply(x, 1, sum)
    * rowMean = apply(x, 1, mean)
    * colSum = apply(x, 2, sum)
    * colMean = apply(x, 2, mean)
  * apply(x, 1, quantile, probs=c(0.25, 0.75))
    * apply for each row: qualtile(row, probs=c(0.25, 0.75)
### tapply ###
  * appliers for a group
  * tapply(X, INDEX, FUN, ...)
  * example
    * x = c(rnorm(10), runif(10), rnorm(10, 1))
    * f = gl(3, 10) `#` 1x10, 2x10, 3x10
    * tapply(x, f, mean) - returns 3 means
### split ###
  * takes a vector and a factor
  * splits it into groups determined by factors
  * apply(X, F, ...)
  * example
    * x = c(rnorm(10), runif(10), rnorm(10, 1))
    * f = gl(3, 10) `#` 1x10, 2x10, 3x10
    * will return 3 groups
### mapply ###
  * multivariable apply
  * mapply(FUN, ...)
  * mapply(rep, 1:4, 4:1)
    * rep(1, 4)
    * rep(2, 3)
    * rep(3, 2)
    * rep(4, 1)

## Debugging ##
### traceback ###
  * prints out stacktrace
### debug ###
  * you can step through
### browser ###
  * suspends execution for debugging
### trace ###
  * allows to insert debug code into existent functions
### recover ###
  * gets the control back


# Week3 #

## Simulations ##
### generating random numbers ###
### common prefixes ###
  * d - density
  * r - random number
  * p - cumulative distribution
  * q - quantile
### normal ###
  * rnorm
  * dnorm
### unified ###
  * runif
  * dunif
### set.seed(n) ###
  * ensures reproducibility
### examples ###
  * linear model
    * y = B`_`0 + B`_`1 `*` X + E
    * E ~ N(0, 4), X ~ N(0, 1),
B`_`0=0.5, B`_`1=2
    * set.seed(20)
    * x = rnorm(100)
    * e = rnorm(100, 0, 2)
    * y = 0.5 + 2 `*` x + e
    * plot(x, y)
### sampling ###
  * sample(1:10, 4) `#` 4 random numbers from 1:10
  * sample(1:10) `#` permutation
  * sample(letters, 5)
  * sample(1:10, replace=TRUE) `#` el can be used multiple times

## Plotting ##
### base ###
  * plot(x, y)
  * params
    * global
      * par - global parameters (?par)
      * example (margins)
        * x = rnorm(100)
        * y = rnorm(100)
        * par(mar=c(2, 2, 2, 2))
        * plot(x, y)
    * plot(x, y, pch=20) `#` solid circles
  * adding
    * title("name") adds a title
    * legend("topleft", legend="Data", pch=20)
    * adding a line
      * fit = lm(y ~ x)
      * abline(fit)
      * abline(fit, lwd=3, color="blue") `#` thick
  * example(points) - built-in demos
  * example
    * plot(x, y, xlab="weight", ylab="height",
main="Scatterplot", pch=20)
    * several plots
      * par(mfraw=c(2, 1))
      * plot(x, y, pch=20)
      * plot(x, z, pch=19)
  * annotations
    * expression
      * plot(0, 0, main=expression(theta==0),
ylab=expression(hat(gamma)==0),
xlab=expression(sum(x[i](i.md)`*`y[i](i.md), i==1,n))
    * substitute
      * xlab=substitute(bar(X)==k, list(k=mean(x))
      * replaces x in the exp onto mean
### lattice ###
  * generally tale a formula
  * y ~ x | f `*` g
    * y ~ x - variables
    * f `*` g - conditional variables
  * returns an object
  * example
    * x = rnorm(100)
    * y = x + rnorm(100, sd=0.5)
    * f = gl(2, 50, labels=c("g1", "g2"))
    * xyplot(x ~ y | f) `#` x vs y, split conditioned on f

## Sorting ##
### sort(read) ###
  * returns a sorted array
### order(read) ###
  * sorts and returns a vector with ordered indexes
    * read[order(read)] == sort(read)
  * order(read, prog) `#` 2 variables
  * order(prog, -read) `#` reverse order
  * order(prog, na.last=F) `#`na goes first
  * order(prog, la.last=NA) `#` na omitted


# Week4 #

## Dates and time ##
### Date class - only dates ###
### Time ###
  * POSIXct
    * based on integer
  * POSIXlt
    * based on list
  * x = Sys.time()
  * p = as.POSIXlt(x)
  * p$sec
### Convertion ###
  * strptime
    * dates = c("January 10, 2012 10:40", "December 9, 2011 10:50")
    * x = strptime(dates, "%B %d, %Y %H:%M")
### Operations ###
  * many operations are allowed: < > + - etc
  * leap years, seconds etc are considered
    * y = as.Date("2012-03-01")
    * x = as.Date("2012-02-28")
    * y - x `#` returns 2
  * and time zones
    * x = as.POSIXct("2012-10-25 01:00:00")
    * y = as.POSIXct("2012-10-25 06:00:00", tz="GMT")
    * x - y = 1 hour

## Reg exps ##
### grep ###
  * grep
    * returns indexes of matches
  * grepl
    * returns T/F vector
### regexpr ###
  * index where match begins, only first
  * gregexpr - all matches
### replacing ###
  * sub
    * sub(pattern, replacement)
    * replaces found pattern on given string
    * only first occurence
  * gsub
    * replaces everything
### regmatches function ###
  * for extracting results returned by regexpr
  * r = regexpr(pattern, h)
  * regexpr(h, r)
### regexec ###
  * regexec(pattern, d)
  * returns list with matches
  * if groups are used, also returned
### example ###
  * r = regexec("<dd><a href='Ff.md'>Ff</a>ound on (.<code>*</code>?)</dd>", homicides)
  * m = regmatches(homicides, r)
  * dates = sapply(m, function(x) x[2](2.md))
  * dates = as.Date(dates, "%B %d, %Y")
  * hist(dates, "year", freq=TRUE)

## Classes and methods ##
### class - description ###
### object - instance of a class ###
### method - operates on a certain class ###
### generic function - for all objets ###
  * don't do anything
  * only dispatches to appropriate function
### more information ###
  * ?Classes, ?Methods, ?setClass, ?setMethod
### S3 classes ###
  * "old-style"
  * mean
    * UseMethod("mean")
    * methods("mean") lists methods for mean
### S4 classes ###
  * "new-style"
  * show
    * StandartGeneric("show")
    * ShowMethod("show")
### dispatching ###
  * gets the class
  * searches for appropriate method
  * if method exists, it's called
  * otherwise default is called
  * if no default, exception is thrown
### example (S4) ###
  * setClass - creates a new class
  * slots - data stored there
    * accessed via @
  * `setClass("polygon", representation(x="numeric", y="numeric"))`
  * 
```
setMethod("plot", "polygon",
	function(x, y, ...) {
		plot(x@x, x@y, type=n, ...)
		xp = c(x@x, x@x[1])
		yp = x(x@y, x@y[1])
		lines(xp, yp)
	}
})
```
  * `p = new("polygon", x=c(1, 2, 3, 4), y=c(1, 2, 3, 1))`
  * plot(p)


# Misc #

## Errors ##
```
if (condition) {
  stop("error message")
}
```

## Proxy ##
Sys.setenv(http`_`proxy="http://username:password@proxyaddress:8080")

## Sets operations ##
  * setdiff(i, j) - elements present in i, not present in j