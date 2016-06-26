## Functions to cache the inverse of a matrix, since inverting a matrix can be
## expensive.

## Creates an object that can store a matrix along with its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(invrs) inv <<- invrs
  getInverse <- function() inv
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


## Returns the inverse of a cache matrix. If the inverse has already been
## cached, it will return the cached value. Otherwise, it will compute the
## inverse, cache it, and then return it.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  m <- x$get()
  inv <- solve(m, ...)
  x$setInverse(inv)
  inv
}
