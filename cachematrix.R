## Assignment: Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
## Your assignment is to write a pair of functions that cache the inverse of a matrix.

## Write the following functions:
  
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(mtx = matrix()) {
  inverse <- NULL
  set <- function(x) {
    mtx <<- x;
    inverse <<- NULL;
  }
  get <- function() return(mtx);
  setinv <- function(solve) inverse <<- solve;
  getinv <- function() return(inverse);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.


cacheSolve <- function(mtx, ...) {
  xinv <- mtx$getinv()
  if (!is.null(xinv))
  {
    print ("Getting cached matrix inverse")
    return (xinv)
  }
  data <- mtx$get()
  xinv <- solve (data)
  
  x$setinv(xinv)
  xinv
}