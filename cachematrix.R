## Two functions for Coursera R programming 
## assignment 2.

## This first function produces a special matrix
## for which it is possible to cache the inverse.
## First one creates a variable with 
## foo <- makeCacheMatrix()
## Then initializes it with, (for example)
## foo$set(matrix( rnorm(16), 4, 4))

makeCacheMatrix <- function(x=matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set= set, get = get, setsolve = setsolve, 
       getsolve = getsolve)
}


## Here we compute the inverse of the matrix,
## and cache the value so it can be accessed
## without computation the next time.  Note that the 
## matrix should be square or else it will return
## an error.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setsolve(m)
  m
}
