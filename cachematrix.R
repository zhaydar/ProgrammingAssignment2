## The code below implements two functions that cache
## the inverse of a matrix rather than compute it repeatedly.
## The <<- operator is used to assign a value to an object
## in an environment that is different from the current environment.

## Two functions are used in conjunction to achieve this.
## The first function, makeCacheMatrix creates a special "matrix" object
## that can cache its inverse.

makecacheMatrix <- function(x = matrix()) {

  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The second function cacheSolve computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve retrieves
## the inverse from the cache.

cacheSolve <- function(x, ...) {

  i <- x$getinverse()
    if(!is.null(i)) {
      message("using cached data")
      return(i)
  }
  message("didn't use cached data")
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
