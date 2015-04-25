## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## (there are also alternatives to matrix inversion that we will not discuss 
## here). 

## Your assignment is to write a pair of functions that cache the inverse of a 
## matrix.
## Write the following functions:

##      makeCacheMatrix: This function creates a special "matrix" object that 
##      can cache its inverse.
##      cacheSolve: This function computes the inverse of the special "matrix" 
##      returned by makeCacheMatrix above. If the inverse has already been 
##      calculated (and the matrix has not changed), then the cachesolve should
##      retrieve the inverse from the cache.

## Computing the inverse of a square matrix can be done with the solve function
## in R. For example, if X is a square invertible matrix, then solve(X) returns
## its inverse.

## For this assignment, assume that the matrix supplied is always invertible.

# =============================================================================

# makeCacheMatrix function, taking a matrix 'x' as its argument (which is to 
# be supplied by the user)

makeCacheMatrix <- function(x = matrix()) {
# Initialise the inverse 'm' to NULL
        m <- NULL
# Define 'set' function, which caches matrix 'x' under 'y' and its inverse as 
# 'm'         
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
# Define 'get' function, which 
        get <- function() x
# Calculate matrix inverse using the 'solve' function
        setSolve <- function(solve) m <<- solve
# Get inverse
        getSolve <- function() m
# Return a list of these functions and where they are cached
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}

# ------------------------------------------------------------------------

# cacheSolve function, which is used to get the inverse from the cache if it is
# there

cacheSolve <- function(x, ...) {
        m <- x$getSolve()
# if the inverse 'm' is not NULL, then return it
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
# if the inverse 'm' is NULL, then calculate it and return it
        data <- x$get()
        m <- solve(data, ...)
        x$setSolve(m)
        m
}
