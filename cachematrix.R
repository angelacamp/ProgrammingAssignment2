## The following functions cache the inverse of a matrix.

## ---------------------------------------------------------------
## makeCacheMatrix creates a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        ## set changes the matrix stored in the main function, makeCacheMatrix
        set <- function(y) {
                x <<- y  ## substitutes matrix x with input y in main function.
                i <<- NULL  ## restores value of inverse i to NULL
        }
        ## get returns the matrix x stored in the main function, makeCacheMatrix.
        get <- function() x
        ## setinverse and get inverse store the value of the input in
        ## a variable i into the main function makeCacheMatrix
        ## (setinverse) and return it (getinverse).
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        ## Store the four functions in the main function makeCacheMatrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## ---------------------------------------------------------------
## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse has already been calculated (and the
## matrix has not changed), then the cacheSolve retrieves the
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## assign x$getinverse() to i
        i <- x$getinverse()
        ## The if statement is checking to see if the inverse has been
        ## calculated for the vector to determine if the inverse can be
        ## retrieved from the cache or calculate the inverse.
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## if the inverse is null, data gets the matrix stored
        ## with makeCacheMatrix to calculate the inverse.
        data <- x$get()
        ## i calculates the inverse of the matrix
        i <- solve(data, ...)
        ## x$setinverse(i) stores it in the object generated assigned
        ## with makeCacheMatrix.
        x$setinverse(i)
        i  ## returns the inverse
}
