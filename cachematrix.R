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
## function will return the inverse of the cached matrix
## as well as attempt to set the value of the inverse of
## the cached matrix using the setinverse() function
## belonging the the cached matrix.


## Creates a list of functions to set and return the
## values of a cached matrix and its index

makeCacheMatrix <- function(x = matrix()) {
        n <- NULL
        set <- function(y) {
                x <<- y
                n <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) n <<- solve
        getinverse <- function() n
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Returns the inverse of a cached matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                n <- x$getinverse()
        if(!is.null(n)) {
                message("getting cached data")
                return(n)
        }
        data <- x$get()
        n <- solve(data, ...)
        x$setinverse(n)
        n
}