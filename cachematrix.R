## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly

## This function creates a special "matrix" object that can cache its inverse.
## the object is a list containing a function to
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  x.inverse <- NULL
  set <- function(y) {
    x <<- y
    x.inverse <<- NULL     # x.inverse is cleared whenever the matrix is changed
  }
  get <- function() x
  set.inverse <- function(inverse) x.inverse <<- inverse
  get.inverse <- function() x.inverse
  list(set = set, get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cacheSolve should retrieve the inverse 
## from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x.inverse <- x$get.inverse()
  if(!is.null(x.inverse)) {
    message("getting cached data")
    return(x.inverse)
  }
  
  ## Calculate the inverse if there is no cache
  ## This can happen, for ex, when x$get() is called
  data <- x$get()
  x.inverse <- solve(data, ...)
  x$set.inverse(x.inverse)
  x.inverse
}
