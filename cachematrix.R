## Create a new matrix object that allows caching of the inverse.
## Assumes that input is an invertible square matrix.

## Makes a new matrix object from a regular matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  y <- list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## Returns the inverse of the matrix stored in a matrix object,
## from cache if possible.  Assumes that matrix is an
## invertible square matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat)
  x$setinverse(inv)
  inv
}
