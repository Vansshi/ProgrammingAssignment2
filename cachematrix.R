## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize inverse as NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset inverse when new matrix is set
  }
  
  get <- function() x  # Function to get matrix
  
  setInverse <- function(inverse) inv <<- inverse  # Function to set inverse
  
  getInverse <- function() inv  # Function to get cached inverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    message("Getting cached data")
    return(inv)  # Return cached inverse if available
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)  # Compute inverse
  x$setInverse(inv)  # Cache the inverse
  
  inv
}
