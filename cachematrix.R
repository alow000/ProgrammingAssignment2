## These two functions are defined below
##
## makeCacheMatrix (matrixX) 
##      Arguments: matrixX - a square matrix (n rows by n columns)
##      Returns: a list with 4 functions: set, get, setSolve, getSolve
##
## cacheSolve (cacheList, ... )
##      Arguments:  cacheList -  a list holding any inverse previous results 
##                              saved from previous calls to function cacheSolve()
##                  ... optional arguments for the standard R function solve()
##
##      Returns:    an inverse for the matrix x pre-defined in cacheList.
##                  
##      Matrix inverse of x returned is 
##          either calculated once if it does not exist in the cache and saved to the cache
##          or if inverse was previously saved in the cache,  cached inverse is returned 
##


## function makeCacheMatrix( x = matrix())
##
## This function initializes a list with 4 functions, based on the matrix x

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## function cachSolve ( x, ...)
## This function returns the inverse of any previously initialized cacheList x

cacheSolve <- function(x, ...) {
    
    ## get any cached version of inverse 
    m <- x$getsolve()
    
    ## if there is one, use it
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ##otherwise get the stored matrix 
    data <- x$get()

    ## compute its inverse 
    m <- solve(data, ...)
    
    ## save it to cache
    x$setsolve(m)
    
    ## returned computed inverse
    m
}

