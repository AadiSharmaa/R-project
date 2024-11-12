## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
# Initialize the cache for the inverse
  inv <- NULL
  
  # Function to set the value of the matrix
  set <- function(y) {
    x <<- y        # Set the matrix value in the parent environment
    inv <<- NULL   # Reset the cached inverse when the matrix is updated
  }
  
  # Function to get the value of the matrix
  get <- function() x
  
  # Function to set the cached inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # Function to get the cached inverse
  getinverse <- function() inv
  
  # Return a list of the functions to access and modify the matrix and its inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         inv <- x$getinverse()
  
  # If the inverse is cached, return it and print a message
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If the inverse is not cached, calculate it
  data <- x$get()                # Get the matrix
  inv <- solve(data, ...)        # Compute the inverse using the solve function
  
  # Cache the computed inverse
  x$setinverse(inv)
  
  # Return the computed inverse
  inv
}
