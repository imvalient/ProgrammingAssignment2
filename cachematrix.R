## makeCacheMatrix
## Function accepts one parametre, a valid square invertible matrix.
## Implements 4 functions for creating a special matrix:
## setMatrix:creates a cached matrix.
## getMatrix: gets the cached matrix.
## setInverse: sets the inverted matrix of the cached matrix.
## getInverse: gets the inverted matrix.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  setMatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  getMatrix <- function() x
  setInverse <- function(inverted) m <<- inverted
  getInverse <- function() m
  list(setMatrix = setMatrix, getMatrix = getMatrix, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## cacheSolve
## Computes the inverse of a matrix passed by parameter.
## if the matrix is alredy computed, returns the result
## without doing the computation.
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return (m)
  }
  
  data <- x$getMatrix()
  m <- solve(data)
  x$setInverse(m, ...)
  m
}
