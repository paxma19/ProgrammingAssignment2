## The following functions cache the inverse of a matrix x.

## this function stores the value of the input in a variable m into the main function makeCacheMatrix (setsolve) and return it (getsolve)

makeCacheMatrix <- function(x = matrix()) {
  m = NULL
  set <- function (y) {
    x <<- y
    m <<- NULL
  } 
  get <- function () x
  setsolve <-function(solve) m <<- solve
  getsolve <- function() m
  list( set = set, get = get, setsolve = setsolve, getsolve = getsolve)
  
}



## This function verifies that the matrix m exists, then if it is stored in memory it returns the matrix m, otherwise it calculates it.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
      