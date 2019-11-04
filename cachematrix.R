## The following solution creates two functions: makeCacheMatrix and cacheSolve

## makeCacheMatrix creates a list containing a function to set the values of 
## the matrix, get the values of the matrix, set the value of the inverse, get 
## the inverse.

makeCacheMatrix <- function(x = matrix()) {
  matrixinv <- NULL
  set <- function(y) {
    x <<- y
    matrixinv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) matrixinv <<- solve
  getinverse <- function() matrixinv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve inverts the matrix. 
## It first checks whether this has already been calculated. 
## If so, it gets the inverted matrix from the cache and skips the computation. 
## Otherwise, it inverts the matrix x and sets this in the cache, via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrixinv <- x$getinverse()
  if(!is.null(matrixinv)) {
    message("getting cached data")
    return(matrixinv)
  }
  data <- x$get()
  matrixinv <- solve(data, ...)
  x$setinverse(matrixinv)
  matrixinv
}
