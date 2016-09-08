## Put comments here that give an overall description of what your
## functions do

## This function provides interface to deal with matrix

makeCacheMatrix <- function(x = matrix()) {
  inversed <- NULL
  set <- function(y) {
    x <<- y
    inversed <<- NULL
  }
  get <- function() x
  setinversed <- function(inversed_) inversed <<- inversed_
  getinversed <- function() inversed
  list(set = set, get = get,
       setinversed = setinversed,
       getinversed= getinversed)
}


## This function inverses matrix in lazy way

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinversed()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinversed(m)
  m
}


## Usage: 
## b <- matrix(c(1,2,3,11), nrow = 2, ncol = 2)
## c <- cacheSolve(makeCacheMatrix(b))
## e <- b %*% c
## e
