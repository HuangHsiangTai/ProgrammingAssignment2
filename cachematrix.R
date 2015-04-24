#This cachematrix.R contains 4 functions. 2 functions including makeCacheMatrix
#and cacheSolve functions are for chached matrix manipulation.  2 functions 
# including test.1 and test.2 functions are for function evaluations


#makeCacheMatrix create a special "matrix" object that can cache its inverse.
#input parameters is a matrix which is going to be cached
#output is the list object where the item is the function 
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    get <- function() {
      x
    }
    set.inverse <- function(i) {
      inverse <<- i
    }
    get.inverse <- function() {
      inverse
    }
    list(set = set, get = get, set.inverse = set.inverse, 
         get.inverse = get.inverse)
}



#cacheSolve will retrieve the inversion of matrix generated from makeCacheMatrix 
#function. It will check if the inversion matrix exist or  not. If the inversion
#matrix exist, it will return the cached inversion matrix. If not, 
#it will calculate the inversion matrix immediately by using solve function
cacheSolve <- function(x, ...) {
  i <- x$get.inverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$set.inverse(i)
  i 
        ## Return a matrix that is the inverse of 'x'
}


## Test basic caching
##
test.1 <- function() {
n <- 3
mat <- matrix(rnorm(1:(n*n)), nrow=n, ncol=n)
matCached <- makeCacheMatrix(mat)
matSolved1 <- cacheSolve(matCached)
matSolved2 <- cacheSolve(matCached)
if (!identical(matSolved1, matSolved2))
  message("Cached version does not match solved version")
}


## Use a time test to see if we really save time
##
test.2 <- function() {
  
  n <- 128
  mat <- matrix(rnorm(1:(n*n)), nrow=n, ncol=n)
  matCached <- makeCacheMatrix(mat)
  time1 <- system.time(matSolved1 <- cacheSolve(matCached))
  time2 <- system.time(matSolved2 <- cacheSolve(matCached))
  if (time1["user.self"] < time2["user.self"])
    message("Solve time is less than cache time")
}