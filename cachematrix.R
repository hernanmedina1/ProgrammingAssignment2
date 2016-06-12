## Programming Assignment 2
## Write closure to either calculate the inverse of a matrix, or get its inverse from the cache

## makeCacheMatrix 
## This function creates a matrix object to cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        getdata <- function() x
        setinverse <- function(zinverse) I <<- zinverse
        getinverse <- function() I
        list(getdata = getdata,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve
## This function computes the inverse of the matrix object returned by makeCacheMatrix above. 
## If the matrix is the same as in the prior call and the inverse has already been calculated, 
## then the function gets the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix I that is the inverse of 'x'
	I <- x$getinverse()
        if(!is.null(I)) {
                message("getting cached inverse")
                return(I)
        }
        zmatrix <- x$getdata()
        I <- solve(zmatrix, ...)
        x$setinverse(I)
        I
}
