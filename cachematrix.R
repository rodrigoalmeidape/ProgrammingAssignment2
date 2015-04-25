# This function creates a special "matrix" object that can cache its inverse, with 4 functions
# to get and set the source matrix and the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL #Any change in the matrix implies in a need to recalculate the matrix inversion.
  }
  # get the matrix stored in x
  get <- function() x
  
  # Store the matrix inversion, which can be passed as an argument 
  # or it can be calculated (as a default) if no argument is passed.
  setsolve <- function(solveArg = solve(x)) m <<- solveArg
  
  # get the matrix inversion stored in memory 
  getsolve <- function() m
  
  # Return a list with 4 functions to get and set the source matrix (get / set)
  # or the matrix inversion (getsolve / setsolve)
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


# This function Computing the inverse of a square matrix, 
# calculating and putting the result in the cache on the first call,
# or taking advantage of what is in cache in subsequent requests 
# with the same matrix.
# Obs: The argument used in this function is a special matrix created with the function "makeCacheMatrix".

cacheSolve <- function(x, ...) {

  m <- x$getsolve()
  # check to see if "setsolve" has been run before, avoiding to do a new calculation 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # Calculate the inverse matrix if it has never been calculated before.
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m) 
  m
}
