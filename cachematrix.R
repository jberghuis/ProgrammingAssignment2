## Matrix inversion is usually a costly computation.
## These functions cache the inverse of a matrix to avoid computing it repeatedly
## if the the contents of the matrix are not changing.

## The function makeCacheMatrix() creates a special "matrix" object that can cache its inverse.
## The input 'x' has to be a square invertible matrix.
## This function returns a list of four functions to be used in the cacheSolve() function.

makeCacheMatrix <- function(x = matrix()) {
  # 'invm' is set to NULL, initializing it as an object within the makeCacheMatrix() environment
  # to be used by later code in the function.
  invm <- NULL
  
  # Define the setter for the matrix 'x'.
  set <- function(y) {
    # Assign the input argument 'y' to the 'x' object in the parent environment
    x <<- y
    # Assign the value of NULL to the 'invm' object in the parent environment
    invm <<- NULL
  }
  
  # Define the getter for the matrix 'x'.
  # 'x' is not defined within get(), but retrieved from the parent environment of makeCacheMatrix()
  get <- function() x
  
  # Define the setter for the inverse 'invm'.
  # Assign the input argument (inverse) to the value of 'invm' in the parent environment.
  setinverse <- function(inverse) invm <<- inverse
  
  # Define the getter for the inverse 'invm'.
  # invm is not defined within getinverse(), but retrieved from the parent environment of makeCacheMatrix()
  getinverse <- function() invm
  
  # Assign each of these four functions as an element within a list(),
  # and return this list to the parent environment.
  list(set = set,          # gives the name 'set' to the set() function defined above
       get = get,          # gives the name 'get' to the get() function defined above
       setinverse = setinverse,  # gives the name 'setinverse' to the setinverse() function defined above
       getinverse = getinverse)  # gives the name 'getinverse' to the getinverse() function defined above
}


## The function cacheSolve() computes the inverse of the special "matrix" 
## returned by makeCacheMatrix() above (the inverse of 'x').
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  # Get the inverse from cache with the getinverse function.       
  invm <- x$getinverse()
  
  # If the inverse is already computed and in cache ('invm' is not NULL)
  if(!is.null(invm)) {
    # then display a message and retrieve the inverse from the cache.  
    message("getting cached data")
    return(invm)
  }
  
  # Otherwise, if the inverse is not in cache ('invm' is NULL), we compute the inverse.
  # We compute the inverse of the matrix with the solve function.
  # The 'b' argument is missing from the solve function, 
  # thus solve will return the inverse of the first argument ('data') to 'invm'.
  data <- x$get()
  invm <- solve(data, ...)
  
  # The inverse is set in cache using the setinverse function.
  x$setinverse(invm)
  
  # The inverse matrix is displayed.
  invm
}
