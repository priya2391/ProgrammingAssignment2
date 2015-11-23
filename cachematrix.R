# This function creates a special matrix object that can  also cache its matrix inverse.
makeCacheMatrix <- function(m = matrix()) {
  i <- NULL 
  ## set function changes the matrix stored in the main function 
  ## <<- operator to assign a value to an object in an environment different from current environment 
  set <- function(x) {
    m <<- x
    i <<- NULL
  }
  ## get function returns the matrix x stored in the main function 
  get <- function()m
  setInverse <- function(inverse) i<<-inverse
  getInverse <- function()i
  ## list is used to assign makeCacheMatrix to an object, which contains 4 functions 
    list(set = set, get=get,
         setInverse=setInverse,
         getInverse=getInverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve retrieves the inverse from the cache.
# Assuming that the matrix supplied is always invertible 
cacheSolve <- function(m, ...){
  i <- m$getInverse()
  ## if condition verifies the value i exists and is not NULL, if it was stored previously using getInverse
  ## if it exists, then it returns the message and the cached value 
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  ## if it is not cached, then data gets the matrix stored with makeCacheMatrix, i calculates matrix inverse using solve function
    data <- m$get()
    i <- solve(data, ...)
    m$setInverse(i)
    i
  }
