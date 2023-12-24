## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverse to NULL
  inv <- NULL  
  
  # Sets the matrix and clears the inverse cache
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Gets the matrix
  get <- function() x
  
  # Sets the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  
  # Gets the inverse of the matrix
  getInverse <- function() inv
  
  # Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # Try to get the cached inverse
  inv <- x$getInverse()  
  if(!is.null(inv)) {
    message("getting cached inverse")
    # Return the cached inverse if it exists
    return(inv)  
  }
  # If the inverse is not cached, calculate it
  # Get the matrix from the special object
  data <- x$get()  
  # Calculate the inverse
  inv <- solve(data, ...)  
  # Cache the inverse for future use
  x$setInverse(inv)  
  # Return the inverse
  inv  
}
