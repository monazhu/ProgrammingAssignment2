## Two functions that output the inverse of a square matrix

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## create inverse variable
  inv <- NULL
  ## creates a square matrix
  set <- function(y = matrix(), nrow=1:Inf, ncol=nrow) {
    x <<- y
    inv <<- NULL
  }
  ## retrieves matrix
  get <- function() x
  
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  ## get matrix inverse if it has already been calculated
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## solve for matrix inverse
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
