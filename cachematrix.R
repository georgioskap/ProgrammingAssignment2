## These two functions will be used for calculating the inverse of a matrix
## using a cache mechanism. The cache mechanism caches the result and if the 
## transpose of the same matrix is requested again the result is returned from the
## cache and not calculated again.

## makeCahceMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

  # Create null object to keep the inverse matrix
  m <- NULL
  
  # Function 1: Set internal variable x to the value of the main matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # Function 2: Get main matrix
  get <- function() x
  
  # Function 3: Set internal variable m to the value of the inverse matrix
  set_inverse <- function(inverse) m <<- inverse
  
  # Function 3: Get inverse matrix
  get_inverse <- function() m
  
  # Return new "special" matrix as list with 4 functions to operate it
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
  
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cacheSolve just retrieves the inverse from the cache.
cacheSolve <- function(x) {
  
  # Check if inverse exist (has been cached) and if yes, return it and exit
  m <- x$get_inverse()
  if(!is.null(m)) {
    message("Getting cached data...")
    return(m)
  }
  
  # Otherwise, get main matrix
  data <- x$get()
  
  # Calculate the inverse of main matrix
  m <- solve(data)
  
  # Cache it for future use
  x$set_inverse(m)
  
  # Return inverse matrix which was just calculated 
  m
  
}
