## This file contains functions for wrapping a matrix with functions that solve
## for its inverse, and caches the result. The cached result optimizes the performance
## of future requests for the matrix's inverse by returning its cached result.

## Wraps a matrix with a list of functions that get and set its data and inverse, 
## and a cache for holdeing the result

makeCacheMatrix <- function(x = matrix()) {

    inverse <- NULL

    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse

    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Solves for the inverse of the wrapped matrix and stores the result in its cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inverse <- x$getInverse()

    if(!is.null(inverse)) {
        message("Getting cached data")
        return(inverse)
    }

    data <- x$get()
    inverse <- solve(data)
    x$setInverse(inverse)

    inverse
}
