## makeCacheMatrix and cacheSolve are a pair of functions intended to be used 
## together to improve the performance of programs that need to repeatedly
## use the inverse of a matrix, they do this by calculating the inverse once 
## and caching the result for subsequent uses.


## makeCacheMatrix() creates a wrapper for a matrix that can be used to cache
## it's inverse. The return value is intended to be used by cacheSolve(). 
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(x) {
        m <<- x
        i <<- NULL
    }
    get <- function() m
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve() calculates the inverse of a matrix, it will only perform the
## calculation once and subsequent calls will used the cached result from the
## first call. The input matrix must be created using the makeCacheMatrix() 
## function.
cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
