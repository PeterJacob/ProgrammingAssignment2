## These two functions work in tandem to cache the result of an inverted matrix.
## The inversion of a matrix can be expensive, so caching can be a good idea.
## Use like this:
## example      <- rbind(c(1, -1/4), c(-1/4, 1))
## exampleListy <- makeCacheMatrix(example)
## cacheSolve(exampleListy)

## makeCacheMatrix creates a list that contains 4 functions to get/set the input matrix,
## and to get/set the inverse matrix along with the stored data.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve returns the inverted matrix, from a cache if that is filled.
## The function does this by reading the function/data list created by makeCacheMatrix,
## and either returning the cached inversion, or computing the inversion, storing it and returning it.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
