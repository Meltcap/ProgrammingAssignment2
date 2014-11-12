## These functions cache the inverse of an invertable matrix
## Use when calculating the inverse of the matrix is time consuming 
## and you need to calculate the inverse multiple times.

## makeCacheMatrix creates an object around a matrix that is able to 
## store and retrieve the inverse of itself.
## set: changes the current matrix 'x' for the new matrix 'y'
## get: gets the current matrix 'x'
## setsolve: sets the cached inverse of the matrix to 'solve'
## getsolve: gets the cached inverse of the matrix or 'NULL' when no cache is set
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(get = get, set = set,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve returns the inverse of a cacheMatrix 'x' created by makeCacheMatrix
## The function first tries to get the cached version. When there is no cached
## version, it calculates the inverse of the cacheMatrix and stores it in the
## cache of the cacheMatrix. Finally the inverse of the matrix is returned.
cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if (!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
