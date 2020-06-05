## function to cache inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  # Set the value of j matrix
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  # Get the value of matrix
  get <- function()x
  
  # Set the value of inverse matrix
  setInverse <- function(inverse) m <<- inverse
  
  # Get the value of inverse matrix
  getInverse <- function() m 
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## compute the inverse of matrix
cacheSolve <- function(x, ...) {
  
  # Return the inverse matrix 'x'
  m <- x$getInverse()
  
  # Check if the inverse of the matrix has already been computed
  # If inverse is already computed, obtain it from cache and return it
  if(!is.null(m)){
    message("getting cached inverse matrix")
    return(m)
  }
  
  # Compute inverse matrix if not already
  mat <- x$get()
  m <- solve(mat,...)
  x$setInverse(m)
  
  m
}
