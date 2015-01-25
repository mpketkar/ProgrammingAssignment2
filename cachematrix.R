## Assignment: Caching the Inverse of a Matrix
## The following pair of functions caches the 
## inverse of a matrix.


## Following are the two functions.
## 1) makeCacheMatrix
## 2) cacheSolve

## Computing the inverse of a square matrix can be done with 
## the solve function in R. For example, if X is a square 
## invertible matrix, then solve(X) returns its inverse.

## For this assignment, it is assumed that the matrix supplied 
##is always invertible.

## makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  ## set the value of the matrix
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() x
  
  ## set the inverse of the matrix
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  
  ## get the inverse of the matrix
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## cacheSolve: This function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above. If 
## the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve would retrieve the 
## inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
  m <- x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}
