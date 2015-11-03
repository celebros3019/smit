## These two functions allow users to cache and retrieve the inverse of given
## matrix "x".

## This function caches the inverse of the given matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list (set = set, get = get, setinv = setinv, getinv = getinv)

}


## This function checks the cache to see if there is a cached value of the
## inverse of a given matrix "x". If so, it returns the cached value. If not,
## it calculates and returns the inverse.


cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  return(m)
}
