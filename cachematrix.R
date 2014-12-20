## The makeCacheMatrix function takes a square invertible
## matrix as its only argument and creates a list of
## functions that set the value of a cached matrix,
## return the value of the cached matrix, set the value
## of the cached matrix's inverse, and return the value of
## the cached matrix's inverse. The default value of the
## inverse is NULL.

## The cacheSolve function takes as its first argument a
## cached matrix (created with the makeCacheMatrix
## function), but it can be passed additional arguments
## that can be used in the call to the solve function
## nested within the cacheSolve function. The cacheSolve
## function will calculate the inverse of the cached
## matrix if it has not already been stored in the cache.
## If the value of the inverse is still NULL, the inverse
## will be calculated and stored to the cache usingthe
## setinverse() function belonging the the cached matrix.


## Creates a list of functions to set and return the
## values of a cached matrix and its index

makeCacheMatrix <- function(x = matrix()) {
        ## Default value of inverse is NULL
        inv_x <- NULL
        
        ## Function to reset the value of the matrix
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        
        ## Function to return the value of the matrix
        get <- function() x
        
        ## Function to set the inverse of the matrix
        setinverse <- function(inverse) inv_x <<- inverse
        
        ##Function to return the value of the inverse
        getinverse <- function() inv_x
        
        ## Return list of above functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Returns the inverse of a cached matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_x <- x$getinverse()
        
        ## Return inverse without recalculating if
        ## inverse has already been stored in cache
        if(!is.null(inv_x)) {
                message("getting cached data")
                return(inv_x)
        }
        
        ## If inverse has not been calculated,
        ## get the value of matrix from cache and
        ## solve for the inverse
        data <- x$get()
        inv_x <- solve(data, ...)
        
        ## Set value of inverse in cache and
        ## return inverse
        x$setinverse(inv_x)
        inv_x
}