#'---
#'author: "Steven Blackstone"
#'date: "18.03.2015"
#'---

## This file defines two functions enabling the caching of matrix inversion computation:
## makeCacheMatrix creates the necessary structure, and cacheSolve does the actual
## computation and caching

## makeCacheMatrix takes a (possibly empty) matrix as input and returns a structure
## consisting of four functions 
##  get: returns the matrix on which computations and caching need to be done
##  set: sets the matrix on which computations and caching need to be done and as a side effect clears the cached value
##  setinverse: sets the value of the inverse matrix - should not be called directly!
##  getinverse: gets the cached value of the inverse matrix
## 
makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(calculatedInverse) cachedInverse <<- calculatedInverse
  getinverse <- function() cachedInverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve operates on a structure as created by the makeCacheMatrix function and
## returns the inverse of the corresponding matrix:
## - first time it will calculate the inverse and cache the result
## - all subsequent times it will return the cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
