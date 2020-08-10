## Matrix inversion is time consuming computation and there may be some 
## benefit to caching the inverse of a matrix rather than repeadely computing it
## Below are two functions that cache the inverse of a matrix.
## It is assumed that the matrix supplied is always invertible, as per instructions.

## The function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The following function calculates the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been estimated (and the 
## matrix remained unchanged), then the cachesolve should retrieve the inverse 
## from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}