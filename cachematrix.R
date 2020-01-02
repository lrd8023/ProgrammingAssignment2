# The makeCacheMatrix function offers multiple
# functions that parse matrix objects and their
# inverse.

makeCacheMatrix <- function(x = matrix()) {
  p <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) p <<- inverse
  getinverse <- function() p
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# The cacheSolve function returns the inverse of a cached matrix object created
# with the above function. However, it first checks to see if the inverse has already
# been calculated. If so, it `get` the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the data sets the value of the inverse via the
# `setinverse` function.
cacheSolve <- function(x, ...) {
  p <- x$getinverse()
  if (!is.null(p)) {
    message("getting cached data")
    return(p)
  }
  data <- x$get()
  p <- solve(data, ...)
  x$setinverse(p)
  p
}
