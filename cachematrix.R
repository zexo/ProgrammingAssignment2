## Coursere R programming Assignment 2
## Inverse of a matrix with results caching

## Creates matrix object for cached inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    if (!identical(x, y)) {
      inv <<- NULL
    }
    x <<- y
  }
  get <- function() { x }
  setsolve <- function(s) { inv <<- s }
  getsolve <- function() { inv }
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  inv <- x$getsolve()
  if(!is.null(inv)) {
    message("getting cached inversed matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv)
  inv
}

