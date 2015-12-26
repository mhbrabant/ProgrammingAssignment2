# makeCacheMatrix creates a list of the following functions:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) cache <<- inverse
  getinverse <- function() cache
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

# cacheSolve returns the inverse of the matrix.
# If the inverse has already been computed, it
# gets the result and skips the computation.
# If not, it computes the inverse, sets the value
# in the cache via the setinverse function.
# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  # if inverse has already been calculated, return cached value
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # if not, calculate it here
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
