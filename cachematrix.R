## We provide two functions that allow defining a "cached" matrix that is able to cache its inverse
## for more efficient calculations.
## We assume all matrices involved are invertible, no checking or error handling is performed.


## This function takes a given matrix and returns a list of functions which
## allow to set and get the underlying matrix as well as getting and settings its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Returns the inverse of the matrix x. If the inverse has been calculated before
## we return it from the cache. Otherwise we calculate it and cache it.
## For demonstration purposes we also print out a message when we retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInverse(m)
    m
}
