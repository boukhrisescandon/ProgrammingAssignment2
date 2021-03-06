## This pair of functions will cache the inverse of a matrix.
## R Programming Course - rprog-033
## October 21, 2015

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvert <- function(solve) m <<- solve
  getinvert <- function() m
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
}


## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the
## cacheSolve function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x["getinvert()"]
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x["get()"]
  m <- solve(data, ...)
  x["setinvert(m)"]
  m
}
