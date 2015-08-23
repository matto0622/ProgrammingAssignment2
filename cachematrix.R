##Contains 2 functions makeCacheMatrix and cacheSolve
##  makeCacheMatrix enables the caching of the inverse of a matrix
##  cacheSolve will cache the inverse of a matrix

## makeCacheMatrix takes a matrix input and creates a vector with the following attributes
##  get() will get the input matrix
##  set() will set the value of the matrix to new matrix
##  getsolve() will returned the cached inverse or null if not calculated
##  setsolve() will set the cached matrix to the value passed as a parameter

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve calculates and caches the inverse of a matrix
##  Input is the vector created by makeCacheMatrix()
##  If cache is already calculated (not null) inverse will be returned


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("Getting cached inverse")
    return(s)
  }
  data <- x$get()
  message("Calculating and caching inverse")
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
