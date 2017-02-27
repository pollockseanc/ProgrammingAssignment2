## These functions solve a matrix and cache the output.
## To correctly use this first create a variable that receves the output of makeCacheMatrix
## then call cacheSolve with that variable as arg

## Returns a list of the functions that can be called by using their name
## Requires a square matrix as input

## Functions:
## set is used to set the matrix and reset the cache
## get retrives the matrix
## setinverse takes the inverse and sets the cache variable to the inverse
## getinverse allows you to retrieve the cached matrix

makeCacheMatrix <- function(x = matrix()) {
        cachemat <- NULL
        
        set <- function(y) {
                x <<- y
                cachemat <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) cachemat <<- inverse
        getinverse <- function() cachemat
        
        arglist <- list (setmatrix = set, getmatrix = get, setinverse = setinverse, getinverse = getinverse)
        arglist       
}


## cacheSolve returns the inverse of a matrix. It requires a list of args to run
## It checks the variable first to see if there is a cached value
## If there is a cached value it returns the the cached matrix
## if the cache is empty then the function retrieves the matrix and solves it, setting the cache variable to the inverse
## Returns the value of the cache value

cacheSolve <- function(x, ...) {
       
        cachemat <- x$getinverse()
        if(!is.null(cachemat)) {
                message("retreiving cached data")
                return(cachemat)
        }
        data <- x$getmatrix()
        cachemat <- solve(data, ...)
        x$setinverse(cachemat)
        cachemat
}
