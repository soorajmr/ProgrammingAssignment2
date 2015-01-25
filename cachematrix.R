###############################################################################
##
## Coursera JHU R programming course, Jan 2015 - programming assignment
##
## Optimized matrix inversion
## This file contains functions to implement repeated inversions of matrices 
## without repeatedly computing the inverse matrices. This is achieved by 
## caching an inverse matrix as it is computed and returning the cached 
## value if the same matrix is attemted to be inverted later.
##
###############################################################################


## This constructor function creates a special "CacheMatrix" object that 
## implements a cache for the inverse of a matrix
##
## Argument: An invertible matrix
##
## Return: 
##      A vector of functions that encapsulates the cache
##      This vector contains these functions:
##        1. get - get the matrix associated with this vector of functions
##        2. getinverse - gets the cached inverse matrix (retrieve from cache)
##        3. setinverse - sets the inverse of the matrix (store in cached)
##
makeCacheMatrix <- function(x = matrix()) {
    ## store the matrix
    m <- x
    
    ## reset the cached inverse variable
    inverse <<- NULL
    
    # function to get the matrix
    getmatrix <- function() m
    
    # function to set the inverse of the matrix
    setinverse <- function(inv) inverse <<- inv
    
    # function to retrieve the cached inverse
    getinverse <- function() inverse
    
    # Now bundle these accessor functions in a list and return
    list(getmatrix = getmatrix, 
         setinverse = setinverse, 
         getinverse = getinverse)
    
}


## An optimized version of "solve" to invert matrices repeatedly
## This function returns a cached inverse matrix if available. If not,
## it caches the computed value for future usage.
##
## Arguments:     x: The list returned by makeCacheMatrix function  
##              ...:  all usual arguments of "solve" function
##
## Returns: inverse of the matrix represented by the list passed
##
## TODO: Handle non-invertible matrices
##
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    
    if(!is.null(inv))
    {
        message("inverse found in cache!")
    }
    else
    {
        # Get the original matrix, compute the inverse and store in cache
        m <- x$getmatrix()
        inv <- solve(m, ...)
        x$setinverse(inv)
    }
    
    # return the inverse matrix
    inv
}


# test code
#m <- matrix(c(2,2,3,2), 2, 2)
#cm <- makeCacheMatrix(m)
#cacheSolve(cm)
#cacheSolve(cm)


