## Put comments here that give an overall description of what your
## functions do

## Return a list of functions described below

makeCacheMatrix <- function(x = matrix()) {
  ## Enclosed matrix inverse value
  inv <- NULL
  ## Function to set the value of the matrix
  set <- function(y) {
      x <<- y
      ## Anytime matrix is set, inverse value is reset to NULL
      inv <<- NULL
  }
  ## Function to get the value of the matrix
  get <- function() x
  ## Function to set the cached value of the matrix inverse
  setInv <- function(i) inv <<- i
  ## Function to get the cached value of the matrix inverse
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## computes the inverse of the matrix returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Get cached inverse value
  inv <- x$getInv()
  ## If it is not empty, use it instead of recomputing  
  if (!is.null(inv)) {
      message("Getting cached data")
      inv
  } else {
  ## Recompute the inverse, cache it, and return the value
      m <- x$get()
      inv <- solve(m)
      x$setInv(inv)
      inv
  }
}
