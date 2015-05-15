##-----------------------------------------------------------------------------
## This script contains the functions:
## 1. makeCacheMatrix
## 2. cacheSolve
##
## cacheSolve inverts a matrix, which has been created
## with the makeCacheMatrix function. It it will return 
## the inverse of the matrix from the current scope if
## it has already been inverted. Otherwise, it will
## be calculated using the solve function.
##
## Example:
## m <- matrix(1:4,2,2)
## mc <- makeCacheMatrix(m)
## cacheSolve(mc)
## output: first run
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## output: after first run
## getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##-----------------------------------------------------------------------------

## makeCacheMatrix create a matrix that can be used to cache the inverse
## of the matrix

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## cacheSolve inverts a matrix, which has been created
## with the makeCacheMatrix function. It it will return 
## the inverse of the matrix from the current scope if
## it has already been inverted. Otherwise, it will
## be calculated using the solve function.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  
}