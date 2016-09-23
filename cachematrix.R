## Set of functions that calculate the inverse of a matrix, but ONLY when 
## it hasn't been already  calculated (and saved in cache).
## This version assumes that the matrix inversion is possible.
##
## Usage:
## sourceMatrix <- matrix( c(2, 1, 5, 3), nrow = 2, ncol = 2)
## myMatrix <- makeCacheMatrix(x = sourceMatrix) # create CacheMatrix object
## cacheSolve(myMatrix) # calculates and returns the inverse (no cache found)
## cacheSolve(myMatrix) # gives message, gets from cache and returns the inverse

## makeCacheMatrix()
## uses lexical scoping to keep track whether a calculation has been done 
## and stored in cache, it has setter and getter functions: set() and get()
## for the matrix to be inversed; setinverseM(), and getinverseM() to set and 
## get the inverse matrix. It returns a list of the functions.
makeCacheMatrix <- function(x = matrix()) {
  # initialize inverted matrix to NULL
  inv <- NULL
  # setter and getter functions for the original data matrix
  # the "<<-" makes the objects persist beyond the function environment
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  # setter and getter functions for the inversed matrix
  setinverseM <- function(inverseM) inv <<- inverseM
  getinverseM <- function() inv
  # return the list of functions
  list(set=set, get=get, setinverseM=setinverseM, getinverseM=getinverseM)
}


## cacheSolve()
## gets as argument the special vector/list created by the function 
## "makeCacheMatrix()". 
## The cacheSolve() function checks whether the calculation has been 
## done and has been stored in cache by calling getinverseM() on the CacheMatrix
## object. If calculated previously, it gives a message and returns the inverse 
## matrix. If not calculated (==NULL), it calculates the inverse, puts it in 
## cache using setinverseM() and returns the inverse Matrix. 
## This version assumes the matrix can be inverted.
cacheSolve <- function(x, ...) {
  # use getter function on object to get the inverse matrix
  # if not NULL, it is there, give message and return the inversed matrix
  inv <- x$getinverseM()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # the inverse matrix doesn't exist yet, get the original matrix,
  # invert it, store it in cache with the setter function of the object,
  # and return the inversed matrix
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverseM(inv)
  inv
}
